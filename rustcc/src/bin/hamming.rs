extern crate bitvec;
use std::io::BufRead;

use bitvec::{prelude::{BitSlice, BitVec, bitvec, BitOrder}, store::BitStore};

fn decoded<T: BitStore, O: BitOrder>(bv: &BitSlice<T, O>) -> i32 {
    let mut databits = BitVec::<T, O>::new();
    let mut res = 0;
    let mut n = 1;
    for i in 1..bv.len() {
        if i == n {
            n *= 2;
            continue;
        }
        databits.push(bv[i]);
    }
    for bit in databits.into_iter() {
        res = (res << 1) + if bit { 1 } else { 0 };
    }
    res
}

fn hamming_check(bv: &mut BitSlice) -> i32 {
    let ck0 = bv.count_ones();
    if (ck0 % 2) == 0 {
        return decoded(bv);
    }
    let mut mask = (0..bv.len()).map(|_| true).collect::<BitVec>();
    let mut i = 1;
    while i < bv.len() && mask.count_ones() > 1 {
        let mut ck = 0;
        let mut bits = BitVec::new();
        for j in i..bv.len() {
            if (j & i) == 0 {
                bits.push(false);
                continue;
            }
            if j < bv.len() {
                if bv[j] {
                    ck += 1;
                }
                bits.push(true);
            }
        }
        if ck % 2 != 0 {
            mask &= bits;
        }
        i *= 2;
    }
    if mask.count_ones() == 1 {
        let mut j = 0;
        for i in 0..bv.len() {
            if mask[i] {
                j = i;
                break;
            }
        }
        bv.set(j, !bv[j]);
    } else {
        assert_eq!(mask.count_ones(), 0);
    }
    decoded(bv)
}

fn main() {
    let mut bv = BitVec::new();
    let mut buf = String::new();
   std::io::stdin().lock().read_line(&mut buf).unwrap();
    for c in buf.chars() {
        if c == '0' {
            bv.push(false);
        } else if c == '1' {
            bv.push(true);
        }
    }
    let res = hamming_check(&mut bv);
    println!("{res}");
}