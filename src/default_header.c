#include <stdlib.h>
#include <stdio.h>

#include "probabilistic.h"

typedef struct abstract_bundle {
	void *func;
	void *data;
} abstract_bundle;

typedef enum {
	FIRST_TYPE_NUM,
	FIRST_TYPE_BOOL,
	FIRST_TYPE_LIST,
	FIRST_TYPE_BUNDLE
} first_type;

typedef struct list_node list_node;

typedef struct list_node {
	int references;
	int first_type;
	union {
		double first_num;
		int first_bool;
		list_node* first_list;
		abstract_bundle first_bundle;
	};
	list_node *rest;
} list_node;



list_node *create_empty_list() {
	return NULL;
}

list_node *create_list_node(list_node *rest) {
	list_node *node = (list_node*) malloc(sizeof(list_node));
	node->references = 1;
	node->rest = rest;
	return node;
}

list_node *create_list_node_num(double x, list_node *rest) {
	list_node *node = create_list_node(rest);
	node->first_type = FIRST_TYPE_NUM;
	node->first_num = x;
	return node;
}

list_node *create_list_node_bool(int x, list_node *rest) {
	list_node *node = create_list_node(rest);
	node->first_type = FIRST_TYPE_BOOL;
	node->first_bool = x;
	return node;
}

list_node *create_list_node_list(list_node *x, list_node *rest) {
	list_node *node = create_list_node(rest);
	node->first_type = FIRST_TYPE_LIST;
	node->first_list = x;
	return node;
}

list_node *create_list_node_bundle(abstract_bundle x, list_node *rest) {
	list_node *node = create_list_node(rest);
	node->first_type = FIRST_TYPE_BUNDLE;
	node->first_bundle = x;
	return node;
}



void delete_list_node(list_node *node) {
	// if (--node->references == 0) {
	// 	if (node->rest != NULL) { delete_list_node(node->rest); }
	// 	free(node);
	// }
}



double first_num(list_node *node) {
	if (node->first_type != FIRST_TYPE_NUM) {
		fprintf(stderr, "Tried to pop a num from list but head element was of a different type.\n");
		exit(EXIT_FAILURE);
	}
	return node->first_num;
}

int first_bool(list_node *node) {
	if (node->first_type != FIRST_TYPE_NUM) {
		fprintf(stderr, "Tried to pop a bool from list but head element was of a different type.\n");
		exit(EXIT_FAILURE);
	}
	return node->first_bool;
}

list_node *first_list(list_node *node) {
	if (node->first_type != FIRST_TYPE_NUM) {
		fprintf(stderr, "Tried to pop a list from list but head element was of a different type.\n");
		exit(EXIT_FAILURE);
	}
	return node->first_list;
}

abstract_bundle first_bundle(list_node *node) {
	if (node->first_type != FIRST_TYPE_BUNDLE) {
		fprintf(stderr, "Tried to pop a function from list but head element was of a different type.\n");
		exit(EXIT_FAILURE);
	}
	return node->first_bundle;
}



list_node *rest(list_node *node) {
	if (node->rest == NULL) {
		fprintf(stderr, "Tried to get tail of empty list.\n");
		exit(EXIT_FAILURE);
	}
	return node->rest;
}

int is_empty_list(list_node *node) {
	return node == NULL;
}

int count_list(list_node *node) {
	int n = 0;
	while (node->rest != NULL) {
		node = node->rest;
		n++;
	}
	return n;
}



double nth_num(list_node *node, double n) {
	while (n > 0) {
		node = rest(node);
		n--;
	}
	return first_num(node);
}

int nth_bool(list_node *node, double n) {
	while (n > 0) {
		node = rest(node);
		n--;
	}
	return first_bool(node);
}

list_node *nth_list(list_node *node, double n) {
	while (n > 0) {
		node = rest(node);
		n--;
	}
	return first_list(node);
}

abstract_bundle nth_bundle(list_node *node, double n) {
	while (n > 0) {
		node = rest(node);
		n--;
	}
	return first_bundle(node);
}



int discrete_rng_wrapper(list_node *list) {
	int list_length = count_list(list);
	double *arr = (double*) malloc(sizeof(double) * list_length);

	int i;
	for (i = 0; i < list_length; i++) {
		arr[i] = first_num(list);
		list = rest(list);
	}

	int ret = discrete_rng(arr, list_length);

	free(arr);

	return ret;
}

double discrete_lnp_wrapper(double value, list_node *list) {
	int list_length = count_list(list);

	if (value < 0 || value >= list_length) {
        return 0;
    }
    else {
        return log(nth_num(list, value));
    }
}



int categorical_num_rng_wrapper(list_node *list) {
	int list_length = count_list(list);
	double *arr = (double*) malloc(sizeof(double) * list_length * 2);

	int i;
	for (i = 0; i < list_length; i++) {
		list_node *node = first_list(list);
		arr[i + list_length] = first_num(rest(node));
		arr[i] = first_num(node);
		list = rest(list);
	}

	int ret = discrete_rng(arr, list_length);

	free(arr);

	return arr[ret + list_length];
}

double categorical_num_lnp_wrapper(double value, list_node *list) {
	int list_length = count_list(list);

	int i;
	for (i = 0; i < list_length; i++) {
		list_node *node = first_list(list);
		if (first_num(node) == value) {
			return log(first_num(rest(node)));
		}
		list = rest(list);
	}

	return 0;
}



