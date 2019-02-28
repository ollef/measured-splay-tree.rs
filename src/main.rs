use num::Zero;
use std::clone::Clone;
use std::ops::Add;

trait Measured<M> {
    fn measure(&self) -> M;
}

impl Measured<usize> for String {
    fn measure(&self) -> usize {
        self.len()
    }
}

enum SplayTree<M, T> {
    Leaf,
    Fork(Box<SplayTreeFork<M, T>>),
}

struct SplayTreeFork<M, T> {
    left: SplayTree<M, T>,
    element: T,
    right: SplayTree<M, T>,
    measure: M,
}

use SplayTree::*;

impl<M, T> Measured<M> for SplayTree<M, T>
where
    M: Clone + Zero,
{
    fn measure(&self) -> M {
        match self {
            Leaf => M::zero(),
            Fork(fork) => (*fork).measure.clone(),
        }
    }
}

enum SplitResult<M, T> {
    NonMonotonic(SplayTree<M, T>, SplayTree<M, T>),
    LeftOf(SplayTree<M, T>),
    RightOf(SplayTree<M, T>),
    Inside(SplayTreeFork<M, T>),
}

impl<M, T> SplayTree<M, T>
where
    T: Measured<M>,
    M: Clone + Zero,
{
    fn fork_measure(l: SplayTree<M, T>, t: T, r: SplayTree<M, T>, m: M) -> SplayTree<M, T> {
        Fork(Box::new(SplayTreeFork {
            left: l,
            element: t,
            right: r,
            measure: m,
        }))
    }
    fn fork(l: SplayTree<M, T>, t: T, r: SplayTree<M, T>) -> SplayTree<M, T> {
        let m = l.measure() + t.measure() + r.measure();
        SplayTree::fork_measure(l, t, r, m)
    }

    fn is_empty(&self) -> bool {
        match self {
            Leaf => true,
            Fork(_) => false,
        }
    }

    fn singleton(t: T) -> SplayTree<M, T> {
        let m = t.measure();
        SplayTree::fork_measure(Leaf, t, Leaf, m)
    }

    fn uncons(self) -> Option<(T, SplayTree<M, T>)> {
        match self {
            Leaf => Option::None,
            Fork(fork_box) => {
                let mut fork = *fork_box;
                loop {
                    match fork.left {
                        Leaf => break Option::Some((fork.element, fork.right)),
                        Fork(left_fork) => {
                            fork = SplayTreeFork {
                                left: left_fork.left,
                                element: left_fork.element,
                                right: SplayTree::fork(left_fork.right, fork.element, fork.right),
                                measure: fork.measure,
                            };
                        }
                    }
                }
            }
        }
    }

    fn unsnoc(self) -> Option<(SplayTree<M, T>, T)> {
        match self {
            Leaf => Option::None,
            Fork(fork_box) => {
                let mut fork = *fork_box;
                loop {
                    match fork.right {
                        Leaf => break Option::Some((fork.left, fork.element)),
                        Fork(right_fork) => {
                            fork = SplayTreeFork {
                                left: SplayTree::fork(fork.left, fork.element, right_fork.left),
                                element: right_fork.element,
                                right: right_fork.right,
                                measure: fork.measure,
                            }
                        }
                    }
                }
            }
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
                Leaf => {
                    break {
                        match (left, right) {
                            (Leaf, Leaf) => {
                                if pred(&v) {
                                    SplitResult::RightOf(Leaf)
                                } else {
                                    SplitResult::LeftOf(Leaf)
                                }
                            }
                            (Leaf, right) => SplitResult::LeftOf(right),
                            (left, Leaf) => SplitResult::RightOf(left),
                            (left, right) => SplitResult::NonMonotonic(left, right),
                        }
                    };
                }

                Fork(fork) => {
                    let vl = v.clone() + fork.left.measure();
                    if pred(&vl) {
                        tree = fork.left;
                        right = SplayTree::singleton(fork.element) + fork.right + right;
                        continue;
                    }
                    let vla = vl.clone() + fork.element.measure();
                    if pred(&vla) {
                        let measure = left.measure() + fork.measure + right.measure();
                        break SplitResult::Inside(SplayTreeFork {
                            left: left + fork.left,
                            element: fork.element,
                            right: fork.right + right,
                            measure: measure,
                        });
                    }
                    v = vla;
                    tree = fork.right;
                    left = left + fork.left + SplayTree::singleton(fork.element);
                }
            }
        }
    }
}

impl<M, T> Add for SplayTree<M, T>
where
    M: Clone + Zero + Add,
    T: Measured<M>,
{
    type Output = SplayTree<M, T>;
    fn add(self, rhs: SplayTree<M, T>) -> SplayTree<M, T> {
        match (self, rhs) {
            (Leaf, rhs) => rhs,
            (lhs, Leaf) => lhs,
            (Fork(left_fork_box), Fork(right_fork_box)) => {
                let mut left_fork = *left_fork_box;
                let mut right_fork = *right_fork_box;
                loop {
                    match (left_fork, right_fork) {
                        (
                            SplayTreeFork {
                                left: left_fork_left,
                                element: left_fork_element,
                                right: Leaf,
                                measure: _,
                            },
                            right_fork,
                        ) => {
                            let measure = left_fork.measure + right_fork.measure.clone();
                            break SplayTree::fork_measure(
                                left_fork_left,
                                left_fork_element,
                                Fork(Box::new(right_fork)),
                                measure,
                            );
                        }
                        (
                            left_fork,
                            SplayTreeFork {
                                left: Leaf,
                                element: right_fork_element,
                                right: right_fork_right,
                                measure: _,
                            },
                        ) => {
                            let measure = left_fork.measure.clone() + right_fork.measure;
                            break SplayTree::fork_measure(
                                Fork(Box::new(left_fork)),
                                right_fork_element,
                                right_fork_right,
                                measure,
                            );
                        }
                        (Fork(mid_left_fork), Fork(mid_right_fork)) => {
                            left_fork = SplayTreeFork {
                                left: SplayTree::fork(
                                    left_fork.left,
                                    left_fork.element,
                                    mid_left_fork.left,
                                ),
                                element: mid_left_fork.element,
                                right: mid_left_fork.right,
                                measure: left_fork.measure,
                            };
                            right_fork = SplayTreeFork {
                                left: mid_right_fork.left,
                                element: mid_right_fork.element,
                                right: SplayTree::fork(
                                    mid_right_fork.right,
                                    right_fork.element,
                                    right_fork.right,
                                ),
                                measure: right_fork.measure,
                            }
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
    println!("{}", r.measure());
    match r.uncons() {
        Option::None => println!("None"),
        Option::Some((s, r)) => println!("Some {} {}", s, r.measure()),
    }
}
