[assume iscloudy (flip 0.5)]

[assume israining (if iscloudy (flip 0.8) (flip 0.2))]

[assume sprinkler (if iscloudy (flip 0.1) (flip 0.5))]

[assume pwetgrass (if (and sprinkler israining) 0.99
                  (if (or  sprinkler israining) 0.9
                                                0))]

[observe (flip pwetgrass) true]

[predict israining]
