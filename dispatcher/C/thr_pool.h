//
//  The Code contained in this file is based on the Thread Pool
//  code examples of the 'Multithreaded Programming Guide' created
//  and made public by Oracle Corporation.
//
//  The guide is available at:
//  http://docs.oracle.com/cd/E19253-01/816-5137/ggedd/index.html
//

/*
 * Declarations for the clients of a thread pool.
 */

#include <pthread.h>

typedef unsigned short ushort;

/*
 * The thr_pool_t type is opaque to the client.
 * It is created by thr_pool_create() and must be passed
 * unmodified to the remainder of the interfaces.
 */
typedef	struct thr_pool	thr_pool_t;
typedef unsigned int job_id;
/*
 * Create a thread pool.
 *	min_threads:	the minimum number of threads kept in the pool,
 *			always available to perform work requests.
 *	max_threads:	the maximum number of threads that can be
 *			in the pool, performing work requests.
 *	linger:		the number of seconds excess idle worker threads
 *			(greater than min_threads) linger before exiting.
 *	attr:		attributes of all worker threads (can be NULL);
 *			can be destroyed after calling thr_pool_create().
 * On error, thr_pool_create() returns NULL with errno set to the error code.
 */
extern	thr_pool_t	*thr_pool_create(ushort min_threads, ushort max_threads,
                                     ushort linger, pthread_attr_t *attr);

/*
 * Enqueue a work request to the thread pool job queue.
 * If there are idle worker threads, awaken one to perform the job.
 * Else if the maximum number of workers has not been reached,
 * create a new worker thread to perform the job.
 * Else just return after adding the job to the queue;
 * an existing worker thread will perform the job when
 * it finishes the job it is currently performing.
 *
 * The job is performed as if a new detached thread were created for it:
 *	pthread_create(NULL, attr, void *(*func)(void *), void *arg);
 *
 * On error, thr_pool_queue() returns -1 with errno set to the error code.
 */
extern	int	thr_pool_queue(thr_pool_t *pool,
                           void *(*func)(void *), void *arg, job_id *id);

extern  int thr_pool_dequeue(thr_pool_t *pool, job_id id);

/*
 * Enqueue a work request to the thread pool prioritized job queue.
 * Jobs added via this function will be prioritized over any new or existing 
 * job enqued using thr_pool_queue. If multiple prioritized jobs are enqued
 * these will be executed in the order they are added.
 *
 * The purpose of prioritized jobs are to ensure that ensure that jobs 
 * with the purpose of generating new jobs, or terminating existing ones
 * are executed as early as possible.
 *
 * If there are idle worker threads, awaken one to perform the job.
 * Else if the maximum number of workers has not been reached,
 * create a new worker thread to perform the job.
 * Else just return after adding the job to the queue;
 * an existing worker thread will perform the job when
 * it finishes the job it is currently performing.
 *
 * The job is performed as if a new detached thread were created for it:
 *	pthread_create(NULL, attr, void *(*func)(void *), void *arg);
 *
 * On error, thr_pool_queue() returns -1 with errno set to the error code.
 */
extern	int	thr_pool_queue_prioritized(thr_pool_t *pool,
                           void *(*func)(void *), void *arg);

/*
 * Wait for all queued jobs to complete.
 */
extern	void	thr_pool_wait(thr_pool_t *pool);

/*
 * Cancel all queued jobs and destroy the pool.
 */
extern	void	thr_pool_destroy(thr_pool_t **poolPtr);
