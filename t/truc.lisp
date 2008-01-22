(in-package #:tpd2.test)

(def-suite truc :in :tpd2)
(in-suite truc)

(test play
  (tpd2.game::launch-game "Truc" (list t t)))

(test robot-sensible
  (tpd2.game::launch-game "Truc" (list (tpd2.game::make-robot-sensible) (tpd2.game::make-robot-sensible))))

(test robot-bully
  (tpd2.game::launch-game "Truc" (list (tpd2.game::make-robot-bully) (tpd2.game::make-robot-bully))))

