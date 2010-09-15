(ns alijn.custom-features-test
  (:use [alijn.custom-features] :reload-all)
  (:use [clojure.test])
  (:import [org.openscience.cdk Atom Bond AtomContainer]))

(deftest hydrogen-donor-and-acceptor
  (let [a0 (Atom. "H")
	a1 (Atom. "O")
	a2 (Atom. "C")
	a3 (Atom. "S")
	a4 (Atom. "H")
	a5 (Atom. "N")
	
	b0 (Bond. a0 a1)
	b1 (Bond. a1 a2)
	b2 (Bond. a1 a3)
	b3 (Bond. a3 a4)
	b4 (Bond. a3 a5)
	
	cont (AtomContainer.)]
    (doto cont
      
      (.addAtom a0)
      (.addAtom a1)
      (.addAtom a2)
      (.addAtom a3)
      (.addAtom a4)
      (.addAtom a5)
      
      (.addBond b0)
      (.addBond b1)
      (.addBond b2)
      (.addBond b3)
      (.addBond b4))

    (is (is-atom? a0 "H"))
    (is (not (is-atom? a1 "H")))
    (is (not (is-atom? a2 "H")))
    (is (not (is-atom? a3 "H")))
    (is (is-atom? a4 "H"))
    (is (not (is-atom? a5 "H")))

    (is (not (is-atom? a0 "O")))
    (is (is-atom? a1 "O"))
    (is (not (is-atom? a2 "O")))
    (is (not (is-atom? a3 "O")))
    (is (not (is-atom? a4 "O")))
    (is (not (is-atom? a5 "H")))

    (is (not (connected-hydrogen? cont a0)))
    (is (connected-hydrogen? cont a1))
    (is (not (connected-hydrogen? cont a2)))
    (is (connected-hydrogen? cont a3))
    (is (not (connected-hydrogen? cont a4)))
    (is (not (connected-hydrogen? cont a5)))
    
    (is (not (is-both-donor-and-acceptor? cont a0)))
    (is (is-both-donor-and-acceptor? cont a1))
    (is (not (is-both-donor-and-acceptor? cont a2)))
    (is (not (is-both-donor-and-acceptor? cont a3)))
    (is (not (is-both-donor-and-acceptor? cont a4)))
    (is (not (is-both-donor-and-acceptor? cont a5)))
	
    (is (not (is-donor? cont a0)))
    (is (is-donor? cont a1))
    (is (not (is-donor? cont a2)))
    (is (is-donor? cont a3))
    (is (not (is-donor? cont a4)))
    (is (not (is-donor? cont a5)))

    (is (not (is-acceptor? cont a0)))
    (is (is-acceptor? cont a1))
    (is (not (is-acceptor? cont a2)))
    (is (not (is-acceptor? cont a3)))
    (is (not (is-acceptor? cont a4)))
    (is (is-acceptor? cont a5))))


