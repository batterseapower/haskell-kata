Section YonedaIsFunctor.

    Inductive yoneda (F : Set -> Set) (A : Set) : Type :=
      | yonedak : (forall (B : Set), (A -> B) -> F B) -> yoneda F A.

    Definition fmap (F : Set -> Set) (A : Set) (B : Set) (f : A -> B) (m : yoneda F A)
      := yonedak F B (fun (C : Set) (k : (B -> C)) => match m with
                                                      | yonedak m => m C (fun (x : A) => k (f x))
                                                      end).
    
    Definition id (A : Set) (x : A) := x.
    
    Theorem first_law : 
      forall (F : Set -> Set) (A : Set) (m : yoneda F A),
        fmap F A A (id A) m = m.
      
    Theorem second_law : 
      forall (F : Set -> Set) (A B C : Set) (f : B -> C) (g : A -> B) (m : yoneda F A),
        fmap F B C f (fmap F A B g m) = fmap F A C (fun (x : A) => f (g x)) m.
