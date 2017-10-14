//
//  The Code contained in this file is based on the Thread Pool
//  code examples of the 'Multithreaded Programming Guide' created
//  and made public by Oracle Corporation.
//
//  The guide is available at:
//  http://docs.oracle.com/cd/E19253-01/816-5137/ggedd/index.html
//

/*
 * Thread pool implementation.
 * See <thr_pool.h> for interface declarations.
 */

#if !defined(_REENTRANT)
#define	_REENTRANT
#endif

#include "thr_pool.h"
#include <stdlib.h>
#include <signal.h>
#include <errno.h>
#include <stdio.h>



/*
 * FIFO queued job
 */
typedef struct job job_t;
struct job {
  job_t	*job_next;             /* linked list of jobs */
  void	*(*job_func)(void *);  /* function to call    */
  void	*job_arg;              /* its argument        */
  job_id id;
};

/*
 * List of active worker threads, linked through their stacks.
 */
typedef struct active active_t;
struct active {
  active_t	*active_next;	/* linked list of threads */
  pthread_t	active_tid;	/* active thread id */
  job_id job;
};

/*
 * The thread pool, opaque to the clients.
 */
struct thr_pool {
  pthread_mutex_t	pool_mutex;     /* protects the pool data */
  pthread_cond_t	pool_busycv;    /* synchronization in pool_queue */
  pthread_cond_t	pool_workcv;    /* synchronization with workers */
  pthread_cond_t	pool_waitcv;    /* synchronization in pool_wait() */
  active_t	*pool_active;           /* list of threads performing work */
  job_t		*pool_head;             /* head of FIFO job queue */
  job_t		*pool_tail;             /* tail of FIFO job queue */
  job_t   *pool_prio_head;          /* head of prioritized job queue */
  job_t   *pool_prio_tail;          /* tail of prioritized job queue */
  pthread_attr_t	pool_attr;      /* attributes of the workers */
  int		pool_flags;             /* see below */
  ushort		pool_linger;        /* seconds before idle workers exit */
  int		pool_minimum;           /* minimum number of worker threads */
  int		pool_maximum;           /* maximum number of worker threads */
  int		pool_nthreads;          /* current number of worker threads */
  int		pool_idle;              /* number of idle workers */
  job_id current_id;                /* most recent used job_id */
};

/* pool_flags */
#define	POOL_WAIT	0x01          /* waiting in thr_pool_wait() */
#define	POOL_DESTROY	0x02      /* pool is being destroyed */

/* set of all signals */
static sigset_t fillset;

static void *worker_thread(void *);

/*
 * Generates the next unique job_id for a given thread pool
 */
job_id
next_id(thr_pool_t *pool) {
  return ++(pool->current_id);
}

static int
create_worker(thr_pool_t *pool)
{
  sigset_t oset;
  int error;
  pthread_t p;

  (void) pthread_sigmask(SIG_SETMASK, &fillset, &oset);
  error = pthread_create(&p, &pool->pool_attr, worker_thread, pool);
  (void) pthread_sigmask(SIG_SETMASK, &oset, NULL);
  return (error);
}

/*
 * Worker thread is terminating.  Possible reasons:
 * - excess idle thread is terminating because there is no work.
 * - thread was cancelled (pool is being destroyed).
 * - the job function called pthread_exit().
 * In the last case, create another worker thread
 * if necessary to keep the pool populated.
 */
static void
worker_cleanup(void *arg)
{
  thr_pool_t *pool = (thr_pool_t *)arg;
  --(pool->pool_nthreads);
  if (pool->pool_flags & POOL_DESTROY) {
    if (pool->pool_nthreads == 0)
      (void) pthread_cond_broadcast(&pool->pool_busycv);
  } else if ((pool->pool_prio_head != NULL
              || pool->pool_head != NULL)
             && pool->pool_nthreads < pool->pool_maximum
             && create_worker(pool) == 0) {
    pool->pool_nthreads++;
  }
  (void) pthread_mutex_unlock(&pool->pool_mutex);
}

static void
notify_waiters(thr_pool_t *pool)
{
  if (pool->pool_prio_head == NULL
      && pool->pool_head == NULL
      && pool->pool_active == NULL) {
    pool->pool_flags &= ~POOL_WAIT;
    (void) pthread_cond_broadcast(&pool->pool_waitcv);
  }
}

/*
 * Called by a worker thread on return from a job.
 */
static void
job_cleanup(void *arg)
{
  thr_pool_t *pool = (thr_pool_t *)arg;
  pthread_t my_tid = pthread_self();
  active_t *activep;
  active_t **activepp;

  (void) pthread_mutex_lock(&pool->pool_mutex);
  for (activepp = &pool->pool_active;
       (activep = *activepp) != NULL;
       activepp = &activep->active_next) {
    if (activep->active_tid == my_tid) {
      *activepp = activep->active_next;
      break;
    }
  }
  if (pool->pool_flags & POOL_WAIT)
    notify_waiters(pool);
}

static void *
worker_thread(void *arg)
{
  thr_pool_t *pool = (thr_pool_t *)arg;
  int timedout;
  job_t *job;
  void *(*func)(void *);
  active_t active;
  struct timespec ts;

  /*
   * This is the worker's main loop.  It will only be left
   * if a timeout occurs or if the pool is being destroyed.
   */
  (void) pthread_mutex_lock(&pool->pool_mutex);
  pthread_cleanup_push(worker_cleanup, pool);
  active.active_tid = pthread_self();
  for (;;) {
    /*
     * We don't know what this thread was doing during
     * its last job, so we reset its signal mask and
     * cancellation state back to the initial values.
     */
    (void) pthread_sigmask(SIG_SETMASK, &fillset, NULL);
    (void) pthread_setcanceltype(PTHREAD_CANCEL_ASYNCHRONOUS, NULL);
    (void) pthread_setcancelstate(PTHREAD_CANCEL_ENABLE, NULL);

    timedout = 0;
    pool->pool_idle++;
    if (pool->pool_flags & POOL_WAIT)
      notify_waiters(pool);
    while (pool->pool_prio_head == NULL
           && pool->pool_head == NULL
           && !(pool->pool_flags & POOL_DESTROY)) {
      if (pool->pool_nthreads <= pool->pool_minimum) {
        (void) pthread_cond_wait(&pool->pool_workcv,
                                 &pool->pool_mutex);
      } else {
        (void) clock_gettime(CLOCK_REALTIME, &ts);
        ts.tv_sec += pool->pool_linger;
        if (pool->pool_linger == 0 ||
            pthread_cond_timedwait(&pool->pool_workcv,
                                   &pool->pool_mutex, &ts) == ETIMEDOUT) {
              timedout = 1;
              break;
            }
      }
    }
    pool->pool_idle--;
    if (pool->pool_flags & POOL_DESTROY)
      break;

    /* First check for prioritized jobs, */
    if ((job = pool->pool_prio_head) != NULL) {
      timedout = 0;
      func = job->job_func;
      arg = job->job_arg;
      pool->pool_prio_head = job->job_next;
      if (job == pool->pool_prio_tail)
        pool->pool_prio_tail = NULL;
      active.active_next = pool->pool_active;
      pool->pool_active = &active;
      active.job = job->id;
      (void) pthread_mutex_unlock(&pool->pool_mutex);
      pthread_cleanup_push(job_cleanup, pool);
      /*
       * Call the specified job function.
       */
      (void) func(arg);
      /*
       * If the job function calls pthread_exit(), the thread
       * calls job_cleanup(pool) and worker_cleanup(pool);
       * the integrity of the pool is thereby maintained.
       */
      pthread_cleanup_pop(1);	/* job_cleanup(pool) */
    }
    /* then check the normal job queue */
    else if ((job = pool->pool_head) != NULL) {
      timedout = 0;
      func = job->job_func;
      arg = job->job_arg;
      pool->pool_head = job->job_next;
      if (job == pool->pool_tail)
        pool->pool_tail = NULL;
      active.active_next = pool->pool_active;
      pool->pool_active = &active;
      active.job = job->id;
      (void) pthread_mutex_unlock(&pool->pool_mutex);
      pthread_cleanup_push(job_cleanup, pool);
      /*
       * Call the specified job function.
       */
      (void) func(arg);
      /*
       * If the job function calls pthread_exit(), the thread
       * calls job_cleanup(pool) and worker_cleanup(pool);
       * the integrity of the pool is thereby maintained.
       */
      pthread_cleanup_pop(1);	/* job_cleanup(pool) */
    }
    if (timedout && pool->pool_nthreads > pool->pool_minimum) {
      /*
       * We timed out and there is no work to be done
       * and the number of workers exceeds the minimum.
       * Exit now to reduce the size of the pool.
       */
      break;
    }
  }
  pthread_cleanup_pop(1);	/* worker_cleanup(pool) */
  return (NULL);
}

thr_pool_t *
thr_pool_create(ushort min_threads, ushort max_threads, ushort linger,
                pthread_attr_t *attr)
{
  thr_pool_t	*pool;

  (void) sigfillset(&fillset);

  if (min_threads > max_threads || max_threads < 1) {
    errno = EINVAL;
    return (NULL);
  }

  if ((pool = malloc(sizeof (*pool))) == NULL) {
    errno = ENOMEM;
    return (NULL);
  }
  (void) pthread_mutex_init(&pool->pool_mutex, NULL);
  (void) pthread_cond_init(&pool->pool_busycv, NULL);
  (void) pthread_cond_init(&pool->pool_workcv, NULL);
  (void) pthread_cond_init(&pool->pool_waitcv, NULL);
  pool->pool_active = NULL;
  pool->pool_head = NULL;
  pool->pool_tail = NULL;
  pool->pool_prio_head = NULL;
  pool->pool_prio_tail = NULL;
  pool->pool_flags = 0;
  pool->pool_linger = linger;
  pool->pool_minimum = min_threads;
  pool->pool_maximum = max_threads;
  pool->pool_nthreads = 0;
  pool->pool_idle = 0;
  pool->current_id = 1;

  (void) pthread_attr_init(&pool->pool_attr);
  /* Create threads whith detach state 'DETACHED' to release resource on thread exit */
  (void) pthread_attr_setdetachstate(&pool->pool_attr, PTHREAD_CREATE_DETACHED);

  return (pool);
}

int
thr_pool_queue(thr_pool_t *pool, void *(*func)(void *), void *arg, job_id *id)
{
  job_t *job;

  if ((job = malloc(sizeof (*job))) == NULL) {
    errno = ENOMEM;
    *id = 0;
    return (-1);
  }
  job->job_next = NULL;
  job->job_func = func;
  job->job_arg = arg;
  job->id = next_id(pool);

  (void) pthread_mutex_lock(&pool->pool_mutex);

  if (pool->pool_head == NULL)
    pool->pool_head = job;
  else
    pool->pool_tail->job_next = job;
  pool->pool_tail = job;

  if (pool->pool_idle > 0)
    (void) pthread_cond_signal(&pool->pool_workcv);
  else if (pool->pool_nthreads < pool->pool_maximum &&
           create_worker(pool) == 0)
    pool->pool_nthreads++;

  *id = job->id;

  (void) pthread_mutex_unlock(&pool->pool_mutex);
  return (0);
}

int
thr_pool_queue_prioritized(thr_pool_t *pool, void *(*func)(void *), void *arg)
{
  job_t *job;

  if ((job = malloc(sizeof (*job))) == NULL) {
    errno = ENOMEM;
    return (-1);
  }
  job->job_next = NULL;
  job->job_func = func;
  job->job_arg = arg;
  job->id = 0;

  (void) pthread_mutex_lock(&pool->pool_mutex);

  if (pool->pool_prio_head == NULL)
    pool->pool_prio_head = job;
  else
    pool->pool_prio_tail->job_next = job;
  pool->pool_prio_tail = job;

  if (pool->pool_idle > 0)
    (void) pthread_cond_signal(&pool->pool_workcv);
  else if (pool->pool_nthreads < pool->pool_maximum &&
           create_worker(pool) == 0)
    pool->pool_nthreads++;

  (void) pthread_mutex_unlock(&pool->pool_mutex);
  return (0);
}


int
thr_pool_dequeue(thr_pool_t *pool, job_id id) {
  job_t *job, *prev = NULL;
  active_t *active;
  
  (void) pthread_mutex_lock(&pool->pool_mutex);
  job = pool->pool_head;
  
  while (job) {
    if (job->id == id) {
      if (pool->pool_head == job) {
        pool->pool_head = job->job_next;
      } else if (prev) {
        prev->job_next = job->job_next;
      }
      (void) pthread_mutex_unlock(&pool->pool_mutex);
      return (0);
    }
    prev = job;
    job = job->job_next;
  }
  
  active = pool->pool_active;
  while (active) {
    if (active->job == id) {
      pthread_cancel(active->active_tid);
      break;
    }
    active = active->active_next;
  }
  
  (void) pthread_mutex_unlock(&pool->pool_mutex);
  
  return (0);
}


void
thr_pool_wait(thr_pool_t *pool)
{
  (void) pthread_mutex_lock(&pool->pool_mutex);
  pthread_cleanup_push(pthread_mutex_unlock, &pool->pool_mutex);
  while (pool->pool_prio_head != NULL
         || pool->pool_head != NULL
         || pool->pool_active != NULL) {
    pool->pool_flags |= POOL_WAIT;
    (void) pthread_cond_wait(&pool->pool_waitcv, &pool->pool_mutex);
  }
  pthread_cleanup_pop(1);	/* pthread_mutex_unlock(&pool->pool_mutex); */
}

void
thr_pool_destroy(thr_pool_t **poolPtr)
{
  active_t *activep;
  job_t *job;
  thr_pool_t * pool = (*poolPtr);
  (void) pthread_mutex_lock(&pool->pool_mutex);
  pthread_cleanup_push(pthread_mutex_unlock, &pool->pool_mutex);

  /* mark the pool as being destroyed; wakeup idle workers */
  pool->pool_flags |= POOL_DESTROY;
  (void) pthread_cond_broadcast(&pool->pool_workcv);

  /* cancel all active workers */
  for (activep = pool->pool_active;
       activep != NULL;
       activep = activep->active_next)
    (void) pthread_cancel(activep->active_tid);

  /* wait for all active workers to finish */
  while (pool->pool_active != NULL) {
    pool->pool_flags |= POOL_WAIT;
    (void) pthread_cond_wait(&pool->pool_waitcv, &pool->pool_mutex);
  }

  /* the last worker to terminate will wake us up */
  while (pool->pool_nthreads != 0)
    (void) pthread_cond_wait(&pool->pool_busycv, &pool->pool_mutex);

  pthread_cleanup_pop(1);	/* pthread_mutex_unlock(&pool->pool_mutex); */

  (void) pthread_attr_destroy(&pool->pool_attr);
  free(pool);
  (*poolPtr) = NULL;
}
