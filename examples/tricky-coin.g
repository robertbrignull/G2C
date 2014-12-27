[assume is-tricky (flip 0.1)]
[assume theta (if is-tricky (beta 1 1) 0.5)]

[observe (flip theta) true]
[observe (flip theta) true]
[observe (flip theta) true]
[observe (flip theta) true]
[observe (flip theta) true]

[predict is-tricky]
[predict theta]
