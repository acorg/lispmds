(in-package user)

(if (and (not (and (boundp '*inhibit-gui*) *inhibit-gui*))
         (or (running-on-windows-p)
             (and
              (not (equal (user-name) "dsmith"))
              (not (equal (user-name) "terry"))
              (not (equal (user-name) "eu"))
              (not (equal (user-name) "ramona"))
              (not (equal (user-name) "_www")))))
    (make-input-ui))
