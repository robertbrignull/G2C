double beta_geometric_lnp(double alpha, double beta, double n, double x) {
	  lgamma(alpha + beta)
	+ lgamma(alpha + n)
	+ lgamma(beta - n + x)
	- lgamma(alpha)
	- lgamma(beta)
	- lgamma(alpha + beta + x);
}
