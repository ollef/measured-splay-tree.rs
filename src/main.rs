use num::Zero;
use std::clone::Clone;
use std::ops::Add;

trait Measured<M> {
    fn measure(&self) -> M;
}

#[derive(Debug)]
enum SplayTree<M, T> {
    Leaf,
    Fork(Box<SplayTreeFork<M, T>>),
}

#[derive(Debug)]
struct SplayTreeFork<M, T> {
    left: SplayTree<M, T>,
    element: T,
    right: SplayTree<M, T>,
    measure: M,
}

use SplayTree::*;

impl<M: Clone + Zero, T> Measured<M> for SplayTree<M, T> {
    fn measure(&self) -> M {
        match self {
            Leaf => M::zero(),
            Fork(fork) => (*fork).measure.clone(),
        }
    }
}

#[derive(Debug)]
enum SplitResult<M, T> {
    NonMonotonic(SplayTree<M, T>, SplayTree<M, T>),
    LeftOf(SplayTree<M, T>),
    RightOf(SplayTree<M, T>),
    Inside(SplayTreeFork<M, T>),
}

enum IteratorItem<'a, M, T> {
    Element(&'a T),
    Tree(&'a SplayTree<M, T>),
}

struct SplayTreeIterator<'a, M, T> {
    todo: Vec<IteratorItem<'a, M, T>>,
}

impl<'a, M, T> Iterator for SplayTreeIterator<'a, M, T> {
    type Item = &'a T;
    fn next(&mut self) -> Option<&'a T> {
        while let Option::Some(item) = self.todo.pop() {
            match item {
                IteratorItem::Element(x) => return Some(x),
                IteratorItem::Tree(Leaf) => {}
                IteratorItem::Tree(Fork(fork)) => {
                    self.todo.push(IteratorItem::Tree(&(*fork).right));
                    self.todo.push(IteratorItem::Element(&(*fork).element));
                    self.todo.push(IteratorItem::Tree(&(*fork).left));
                }
            }
        }
        return Option::None;
    }
}

impl<'a, M, T> From<&'a SplayTree<M, T>> for SplayTreeIterator<'a, M, T> {
    fn from(tree: &'a SplayTree<M, T>) -> SplayTreeIterator<'a, M, T> {
        SplayTreeIterator {
            todo: vec![IteratorItem::Tree(tree)],
        }
    }
}

impl<M: Clone + Zero, T: Measured<M>> SplayTree<M, T> {
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

    fn split<F: Fn(&M) -> bool>(self, pred: F) -> SplitResult<M, T> {
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
                        right = SplayTree::from(fork.element) + fork.right + right;
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
                    left = left + fork.left + SplayTree::from(fork.element);
                }
            }
        }
    }

    fn iter(&self) -> SplayTreeIterator<M, T> {
        SplayTreeIterator::from(self)
    }
}

impl<M: Clone + Zero, T: Measured<M>> From<T> for SplayTree<M, T> {
    fn from(t: T) -> SplayTree<M, T> {
        let m = t.measure();
        SplayTree::fork_measure(Leaf, t, Leaf, m)
    }
}

impl<M: Clone + Zero + Add, T: Measured<M>> Add for SplayTree<M, T> {
    type Output = SplayTree<M, T>;
    fn add(self, rhs: SplayTree<M, T>) -> SplayTree<M, T> {
        match (self, rhs) {
            (Leaf, rhs) => rhs,
            (lhs, Leaf) => lhs,
            (Fork(left_fork_box), Fork(right_fork_box)) => {
                let mut left_fork = *left_fork_box;
                let mut right_fork = *right_fork_box;
                loop {
                    match (left_fork.right, right_fork.left) {
                        (Leaf, right_fork_left) => {
                            let measure = left_fork.measure + right_fork.measure.clone();
                            break SplayTree::fork_measure(
                                left_fork.left,
                                left_fork.element,
                                SplayTree::fork_measure(
                                    right_fork_left,
                                    right_fork.element,
                                    right_fork.right,
                                    right_fork.measure,
                                ),
                                measure,
                            );
                        }
                        (left_fork_right, Leaf) => {
                            let measure = left_fork.measure.clone() + right_fork.measure;
                            break SplayTree::fork_measure(
                                SplayTree::fork_measure(
                                    left_fork.left,
                                    left_fork.element,
                                    left_fork_right,
                                    left_fork.measure,
                                ),
                                right_fork.element,
                                right_fork.right,
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

//-----------------------------------------------------------------------------

#[derive(Clone, PartialEq, Eq, Debug)]
struct StringMeasure {
    len: usize,
    char_count: usize,
    newline_count: usize,
}

impl StringMeasure {
    fn from(s: &MeasuredString) -> StringMeasure {
        StringMeasure {
            len: s.string.len(),
            char_count: s.char_count,
            newline_count: s.newline_count,
        }
    }
}

impl Add for &StringMeasure {
    type Output = StringMeasure;

    fn add(self, rhs: &StringMeasure) -> StringMeasure {
        StringMeasure {
            len: self.len + rhs.len,
            char_count: self.char_count + rhs.char_count,
            newline_count: self.newline_count + rhs.newline_count,
        }
    }
}

impl Add for StringMeasure {
    type Output = StringMeasure;

    fn add(self, rhs: StringMeasure) -> StringMeasure {
        &self + &rhs
    }
}

impl Zero for StringMeasure {
    fn is_zero(&self) -> bool {
        self == &StringMeasure::zero()
    }

    fn zero() -> StringMeasure {
        StringMeasure {
            len: 0,
            char_count: 0,
            newline_count: 0,
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
struct MeasuredString {
    string: String,
    char_count: usize,
    newline_count: usize,
}

impl From<String> for MeasuredString {
    fn from(s: String) -> MeasuredString {
        let char_count = s.chars().count();
        let newline_count = s.match_indices('\n').count();
        MeasuredString {
            string: s,
            char_count: char_count,
            newline_count: newline_count,
        }
    }
}

impl Measured<StringMeasure> for MeasuredString {
    fn measure(&self) -> StringMeasure {
        StringMeasure::from(self)
    }
}

impl Add for &MeasuredString {
    type Output = MeasuredString;

    fn add(self, rhs: &MeasuredString) -> MeasuredString {
        (*self).clone() + (*rhs).clone()
    }
}

impl Add for MeasuredString {
    type Output = MeasuredString;

    fn add(self, rhs: MeasuredString) -> MeasuredString {
        let char_count = self.char_count + rhs.char_count;
        let newline_count = self.newline_count + rhs.newline_count;
        MeasuredString {
            string: self.string + &rhs.string,
            char_count: char_count,
            newline_count: newline_count,
        }
    }
}

#[derive(Debug)]
struct Rope(SplayTree<StringMeasure, MeasuredString>);
const CHUNK_SIZE: usize = 4096;

impl Rope {
    fn new() -> Rope {
        Rope(Leaf)
    }

    fn to_string(&self) -> String {
        let Rope(tree) = self;
        let mut result = String::with_capacity(tree.measure().len);
        for s in tree.iter() {
            result.push_str(&s.string)
        }
        result
    }
}

impl From<String> for Rope {
    fn from(s: String) -> Rope {
        if s.is_empty() {
            Rope(Leaf)
        } else {
            Rope(SplayTree::from(MeasuredString::from(s)))
        }
    }
}

impl Add for Rope {
    type Output = Rope;
    fn add(self, rhs: Rope) -> Rope {
        let Rope(left_tree) = self;
        let Rope(right_tree) = rhs;
        match (left_tree.unsnoc(), right_tree.uncons()) {
            (Option::None, Option::None) => Rope::new(),
            (Option::Some((left, left_str)), Option::None) => {
                Rope(left + SplayTree::from(left_str))
            }
            (Option::None, Option::Some((right_str, right))) => {
                Rope(SplayTree::from(right_str) + right)
            }
            (Option::Some((left, left_str)), Option::Some((right_str, right))) => {
                if left_str.string.len() + right_str.string.len() <= CHUNK_SIZE {
                    Rope(left + SplayTree::from(left_str + right_str) + right)
                } else {
                    Rope(left + (SplayTree::from(left_str) + SplayTree::from(right_str)) + right)
                }
            }
        }
    }
}

fn main() {
    let r = Rope::from("Hello".to_string()) + Rope::from(", world!!".to_string());
    println!("{:?}", r);
    println!("{}", r.to_string());
}
