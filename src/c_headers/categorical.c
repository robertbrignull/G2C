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
