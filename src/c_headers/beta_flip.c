double beta_flip_lnp(double alpha, double beta, double n, double x) {
	return
	  lgamma(n + 1)
	+ lgamma(alpha + beta)
	+ lgamma(alpha + x)
	+ lgamma(beta + n - x)
	- lgamma(alpha)
	- lgamma(beta)
	- lgamma(x + 1)
	- lgamma(n - x + 1)
	- lgamma(alpha + beta + n);
}
