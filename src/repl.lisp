(in-package :mongoose)

#+nil
(let ((mgr (make-alien mgr)))
  (mgr-init mgr)
  (mgr-free mgr))
