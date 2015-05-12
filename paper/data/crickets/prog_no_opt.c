#include <stdlib.h>
#include <stdio.h>

#include "probabilistic.h"

int main(int argc, char **argv) {
    double gradient = uniform_rng(0.0, 1.0);

    double coeff = normal_rng(gradient, 0.05);
    double const_ = normal_rng(0.0, 0.2);

    observe(normal_lnp(20.0, coeff * 88.6 + const_, 0.1));
    observe(normal_lnp(16.0, coeff * 71.6 + const_, 0.1));
    observe(normal_lnp(19.8, coeff * 93.3 + const_, 0.1));
    observe(normal_lnp(18.4, coeff * 84.3 + const_, 0.1));
    observe(normal_lnp(17.1, coeff * 80.6 + const_, 0.1));
    observe(normal_lnp(15.5, coeff * 75.2 + const_, 0.1));

    predict("%s,%f\n", "gradient", gradient);

    return 0;
}