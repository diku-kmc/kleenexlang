#ifndef VECTORS
#define VECTORS
#define VECTOR_INITIAL_CAPACITY 100

// Node pointer vector
typedef struct node_vector {
    node** data;
    int len;
    int capacity;
} node_vector;

node_vector* nvector_create() {
    node_vector* nvector = (node_vector*) malloc(sizeof(node_vector));
    node** narray         = (node**) malloc(VECTOR_INITIAL_CAPACITY * sizeof(node*));

    nvector->data        = narray;
    nvector->len         = 0;
    nvector->capacity    = VECTOR_INITIAL_CAPACITY;

    return nvector;
}

void nvector_free(node_vector* vec) {
    free(vec->data);
    free(vec);
}

void nvector_grow(node_vector* vec, int min_capacity) {
    if (vec->capacity < min_capacity) {
        vec->capacity += min_capacity;
        vec->data = realloc(vec->data, sizeof(node*) * vec->capacity);
    }
    return;
}

void nvector_insert(node_vector* vec, node* data, int ind) {
    nvector_grow(vec, vec->len + 1);
    memmove(vec->data + ind + 1, vec->data + ind, (vec->len - ind) * sizeof(node*));
    vec->data[ind] = data;
    vec->len++;
}

void nvector_append(node_vector* vec, node* data) {
    nvector_grow(vec, vec->len + 1);
    vec->data[vec->len] = data;
    vec->len++;
}

void nvector_remove(node_vector* vec, int ind) {
    if (vec->len < 1)
        exit(30); // Removing element from empty vector

    memmove(vec->data + ind, vec->data + ind + 1, (vec->len - (ind + 1)) * sizeof(node*));
    vec->len--;
}

node* nvector_pop(node_vector* vec) {
    if (vec->len < 1) printf("WHAT!!!");
    vec->len -= 1;
    return vec->data[vec->len];
}

void nvector_push(node_vector* vec, node* data) {
    nvector_grow(vec, vec->len + 1);
    vec->data[vec->len] = data;
    vec->len += 1;
}


// Concatenates two vectors in to one, and frees the remaining vector.
void nvector_concat(node_vector* vec1, node_vector* vec2) {
    nvector_grow(vec1, vec1->len + vec2->len);
    memcpy(vec1->data + (vec1->len - 1), vec2->data, (vec2->len) * sizeof(node*));
    vec1->len += vec2->len;
    nvector_free(vec2);
}


// Char vector
struct char_vector {
    char* data;
    int len;
    int capacity;
};

char_vector* cvector_create() {
    char_vector* vector = (char_vector*) malloc(sizeof(char_vector));
    char* array         = (char*) malloc(VECTOR_INITIAL_CAPACITY * sizeof(char));

    if (array == NULL)
        exit(666);
    vector->data        = array;
    vector->len         = 0;
    vector->capacity    = VECTOR_INITIAL_CAPACITY;

    return vector;
}

void cvector_free(char_vector* vec) {
    free(vec->data);
    free(vec);
}

void cvector_grow(char_vector* vec, int min_capacity) {
    if (vec->capacity < min_capacity) {
        vec->capacity += min_capacity;
        vec->data = realloc(vec->data, sizeof(char) * vec->capacity);
    }
    return;
}

void cvector_insert(char_vector* vec, char data, int ind) {
    cvector_grow(vec, vec->len + 1);
    memmove(vec->data + ind + 1, vec->data + ind, (vec->len - ind) * sizeof(char));
    vec->data[ind] = data;
    vec->len++;
}

void cvector_append(char_vector* vec, char data) {
    cvector_grow(vec, vec->len + 1);
    vec->data[vec->len] = data;
    vec->len++;
}

void cvector_remove(char_vector* vec, int ind) {
    if (vec->len < 1)
        exit(30); // Removing element from empty vector

    memmove(vec->data + ind, vec->data + ind + 1, (vec->len - (ind + 1)) * sizeof(char));
    vec->len--;
}

void cvector_concat(char_vector* vec1, char_vector* vec2) {
    cvector_grow(vec1, vec1->len + vec2->len);
    memcpy(vec1->data + (vec1->len - 1), vec2->data, (vec2->len) * sizeof(char));
    vec1->len += vec2->len;
}

void cvector_prepend(char_vector* vec1, char_vector* vec2) {
    if (vec1->len > 0) {
        cvector_grow(vec2, vec1->len + vec2->len);
        memcpy(vec2->data + vec2->len, vec1->data, (vec1->len) * sizeof(char));
    }
    vec1->len += vec2->len;

    // Switch data pointer and capacity.
    char* tmp_data = vec1->data;
    int   tmp_cap = vec1->capacity;
    vec1->data = vec2->data;
    vec1->capacity = vec2->capacity;
    vec2->data = tmp_data;
    vec2->capacity = tmp_cap;
}
#endif
