double merged_normal_observes_lnp(double mu, double variance, double n, double sample_mean, double sample_variance_times_n) {
	return
	- n / 2. * log(2. * M_PI * variance)
	- 1. / (2. * variance) * (
		  sample_variance_times_n
		+ n * (mu - sample_mean) * (mu - sample_mean)
	);
}
