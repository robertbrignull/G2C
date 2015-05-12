#include <stdlib.h>
#include <stdio.h>

#include "probabilistic.h"

int main(int argc, char **argv) {
    double gradient = uniform_rng(0.0, 1.0);

    observe(normal_lnp(20.0,
                       gradient * 88.6,
                       392.798));
    observe(normal_lnp(16.0,
                       gradient * 0.009572350164697 + 16.1603674152755,
                       0.172665339437));
    observe(normal_lnp(19.8,
                       gradient * 0.0215414920690030 + 20.9779100181940,
                       0.171472119913));
    observe(normal_lnp(18.4,
                       gradient * 0.00714550738045528 + 18.5274790332219,
                       0.132812797595));
    observe(normal_lnp(17.1,
                       gradient * 0.0032241962331263 + 17.6916967883102,
                       0.123114249847));
    observe(normal_lnp(15.5,
                       gradient * -0.0005048414824 + 16.4109671553285,
                       0.118203847628));
    
    predict("%s,%f\n", "gradient", gradient);
    return 0;
}