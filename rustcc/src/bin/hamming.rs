extern crate bitvec;
use std::io::BufRead;

use bitvec::{prelude::{BitSlice, BitVec, bitvec, BitOrder, Lsb0}, store::BitStore};

fn decoded<T: BitStore, O: BitOrder>(bv: &BitSlice<T, O>) -> i64 {
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

fn hamming_check(bv: &mut BitSlice) -> i64 {
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

fn encode(buf: &str) -> String {
    let value = buf.trim().parse::<i64>().expect("Not a valid integer");
    let mut value_bits = bitvec![];
    let mut tmp = value;
    while tmp > 0 {
        value_bits.push((tmp & 1) == 1);
        tmp >>= 1;
    }
    value_bits.reverse();
    let l = value_bits.len();
    let mut n_parity = 0;
    while (1 << n_parity) < l + n_parity + 1 {
        n_parity += 1;
    }
    let mut bv = bitvec![0; l + n_parity + 1];
    let mut i = 1;
    let mut j = 0;
    while i < bv.len() {
        if (i & (i - 1)) == 0 {
            i += 1;
            continue;
        }
        bv.set(i, value_bits[j]);
        i += 1;
        j += 1;
    }

    for i in 0..n_parity {
        let mut ck = false;
        let ckix = 1 << i;
        for j in 1..bv.len() {
            if (j & ckix) != 0 {
                ck ^= bv[j];
            }
        }
        bv.set(ckix, ck);
    }
    let ck0 = bv.count_ones() % 2 == 1;
    bv.set(0, ck0);
    let mut res = String::new();
    for bit in bv.into_iter() {
        res.push(if bit { '1' } else { '0' });
    }
    res
}

fn decode(buf: &str) -> String {
   let mut bv = BitVec::new();
    for c in buf.chars() {
        if c == '0' {
            bv.push(false);
        } else if c == '1' {
            bv.push(true);
        }
    }
    let res = hamming_check(&mut bv);
    format!("{}", res)
}

fn main() {
    let mut buf = String::new();
    std::io::stdin().lock().read_line(&mut buf).unwrap();
    let data = buf.strip_suffix('\n').unwrap_or_else(|| &buf);
    let res = if is_binary(data) {
        decode(data)
    } else {
        encode(data)
    };
    println!("{res}");
}

fn is_binary(buf: &str) -> bool {
    buf.chars().all(|c| c == '0' || c == '1')
}
