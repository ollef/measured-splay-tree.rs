use num::Zero;
use std::clone::Clone;
use std::ops::Add;
use std::rc::Rc;

trait Measured<M> {
    fn measure(&self) -> M;
}

impl Measured<usize> for String {
    fn measure(&self) -> usize {
        self.len()
    }
}

#[derive(Clone)]
enum SplayTree<M, T> {
    Leaf,
    Fork(Rc<SplayTree<M, T>>, T, Rc<SplayTree<M, T>>, M),
}

use SplayTree::*;

impl<M, T> Measured<M> for SplayTree<M, T>
where
    M: Clone + Zero,
{
    fn measure(&self) -> M {
        match self {
            Leaf => M::zero(),
            Fork(_, _, _, m) => m.clone(),
        }
    }
}

fn fork<M, T>(l: &Rc<SplayTree<M, T>>, t: &T, r: &Rc<SplayTree<M, T>>) -> Rc<SplayTree<M, T>>
where
    T: Measured<M> + Clone,
    M: Clone + Zero,
{
    let m = (*l).measure() + t.measure() + (*r).measure();
    Rc::new(Fork(l.clone(), (*t).clone(), r.clone(), m))
}

enum SplitResult<M, T> {
    Outside,
    Inside(SplayTree<M, T>, T, SplayTree<M, T>),
}

impl<M, T> SplayTree<M, T>
where
    T: Measured<M> + Clone,
    M: Clone + Zero,
{
    fn is_empty(&self) -> bool {
        match self {
            Leaf => true,
            Fork(_, _, _, _) => false,
        }
    }

    fn singleton(t: T) -> SplayTree<M, T> {
        let m = t.measure();
        Fork(Rc::new(Leaf), t, Rc::new(Leaf), m)
    }

    fn uncons(self) -> Option<(T, SplayTree<M, T>)> {
        match self {
            Leaf => Option::None,
            Fork(mut l, mut a, mut r, _) => loop {
                match &*l {
                    Leaf => break Option::Some((a.clone(), (*r).clone())),
                    Fork(l2, a2, m, _) => {
                        r = fork(m, &a, &r);
                        a = a2.clone();
                        l = l2.clone();
                    }
                }
            },
        }
    }

    fn unsnoc(self) -> Option<(SplayTree<M, T>, T)> {
        match self {
            Leaf => Option::None,
            Fork(mut l, mut a, mut r, _) => loop {
                match &*r {
                    Leaf => break Option::Some(((*l).clone(), a.clone())),
                    Fork(m, a2, r2, _) => {
                        l = fork(&l, &a, m);
                        a = a2.clone();
                        r = r2.clone();
                    }
                }
            },
        }
    }

    fn split<F>(self, pred: F) -> SplitResult<M, T>
    where
        F: Fn(&M) -> bool,
    {
        let mut v = M::zero();
        let mut left = Leaf;
        let mut tree = self;
        let mut right = Leaf;
        loop {
            match tree {
                Leaf => break SplitResult::Outside,
                Fork(l, a, r, _) => {
                    let vl = v.clone() + l.measure();
                    if pred(&vl) {
                        tree = (*l).clone();
                        right = SplayTree::<M, T>::singleton(a) + (*r).clone() + right;
                        continue;
                    }
                    let vla = vl.clone() + a.measure();
                    if pred(&vla) {
                        break SplitResult::Inside(left + (*l).clone(), a, (*r).clone() + right);
                    }
                    v = vla;
                    tree = (*r).clone();
                    left = left + (*l).clone() + SplayTree::<M, T>::singleton(a);
                }
            }
        }
    }
}

impl<M, T> Add for SplayTree<M, T>
where
    M: Clone + Zero + Add,
    T: Measured<M> + Clone,
{
    type Output = SplayTree<M, T>;
    fn add(self, rhs: SplayTree<M, T>) -> SplayTree<M, T> {
        match (self, rhs) {
            (Leaf, rhs) => rhs,
            (lhs, Leaf) => lhs,
            (Fork(mut l1, mut a1, mut r1, mut lar1), Fork(mut l2, mut a2, mut r2, mut lar2)) => {
                loop {
                    match (&*r1, &*l2) {
                        (Leaf, _) => {
                            break Fork(
                                l1,
                                a1,
                                Rc::new(Fork(l2, a2, r2, lar2.clone())),
                                lar1 + lar2,
                            );
                        }

                        (_, Leaf) => {
                            let r1_measure = r1.measure();
                            break Fork(
                                l1,
                                a1,
                                Rc::new(Fork(r1.clone(), a2, r2, r1_measure + lar2.clone())),
                                lar1 + lar2,
                            );
                        }
                        (Fork(lr1, ar1, rr1, _), Fork(ll2, al2, rl2, _)) => {
                            l1 = fork(&l1, &a1, &lr1);
                            a1 = ar1.clone();
                            r1 = rr1.clone();
                            lar1 = l1.measure() + a1.measure() + r1.measure();
                            r2 = fork(&rl2, &a2, &r2);
                            a2 = al2.clone();
                            l2 = ll2.clone();
                            lar2 = l2.measure() + a2.measure() + r2.measure();
                        }
                    }
                }
            }
        }
    }
}

type Rope = SplayTree<usize, String>;

fn main() {
    let r = Rope::singleton("Hello".to_string()) + Rope::singleton(", world!!".to_string());
    println!("{}", r.clone().measure());
    match r.uncons() {
        Option::None => println!("None"),
        Option::Some((s, r)) => println!("Some {} {}", s, r.measure()),
    }
}
