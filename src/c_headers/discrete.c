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
