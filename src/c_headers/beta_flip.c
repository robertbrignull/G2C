double beta_flip_lnp(double alpha, double beta, double n, double x) {
	return
	  lgamma(alpha + x)
	+ lgamma(beta + n - x)
	- lgamma(alpha + beta + n)
	- lgamma(alpha)
	- lgamma(beta)
	+ lgamma(alpha + beta);
}
