[assume battery-age 6]

[assume p-battery-dead (- 1 (exp (- 0 (/ battery-age 5))))]
[assume battery-dead (flip p-battery-dead)]

[assume p-battery-meter (if battery-dead 0.05 0.95)]
[assume battery-meter (flip p-battery-meter)]

[assume p-alternator-broken 0.01]
[assume alternator-broken (flip p-alternator-broken)]

[assume p-fanbelt-broken 0.01]
[assume fanbelt-broken (flip p-fanbelt-broken)]

[assume p-no-charging (if (or alternator-broken fanbelt-broken) 1 0.2)]
[assume no-charging (flip p-no-charging)]

[assume p-battery-flat (if battery-dead 1 (if no-charging 0.9 0.05))]
[assume battery-flat (flip p-battery-flat)]

[assume p-no-oil 0.05]
[assume no-oil (flip p-no-oil)]

[assume p-no-gas 0.1]
[assume no-gas (flip p-no-gas)]

[assume p-fuel-line-blocked 0.01]
[assume fuel-line-blocked (flip p-fuel-line-blocked)]

[assume p-starter-broken 0.2]
[assume starter-broken (flip p-starter-broken)]

[assume p-lights (if battery-flat 0 0.7)]
[assume lights (flip p-lights)]

[assume p-oil-light (if battery-flat 0 (if no-oil 0.9 0.6))]
[assume oil-light (flip p-oil-light)]

[assume p-dipstick (if no-oil 0 0.9)]
[assume dipstick (flip p-dipstick)]

[assume p-gas-light (if battery-flat 0 (if no-gas 0.9 0.2))]
[assume gas-light (flip p-gas-light)]

[assume p-car-wont-start (if (or battery-flat (or no-oil (or no-gas (or fuel-line-blocked starter-broken)))) 1 0.1)]

[observe (flip p-battery-meter) false]
[observe (flip p-oil-light) true]
[observe (flip p-dipstick) true]
[observe (flip p-lights) false]
[observe (flip p-gas-light) false]
[observe (flip p-car-wont-start) true]

[predict fanbelt-broken]
[predict battery-flat]
[predict no-gas]
