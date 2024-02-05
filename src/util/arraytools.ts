/**
 * Lightweight function for shallow array comparison-by-value. If the objects within the array are
 * compare-by-value, this works. Otherwise, and for arrays-of-arrays, a different function should be used instead.
 * @param a {Array<T>} First array
 * @param b {Array<T>} Second array
 * @returns {boolean} True if the arrays are equal, false otherwise
 */
export function arrayEq<T>(a: T[], b: T[]): boolean {
    if (a.length !== b.length) return false;
    for (let i = 0; i < a.length; i++) {
        if (a[i] !== b[i]) return false;
    }
    return true;
}


/**
 * Returns a sorted copy of an array with all duplicate elements removed
 *
 * @param a
 * @returns
 */
export function uniqSort<T>(a: T[], compareFn?: ((a: T, b: T) => number), eqFn = ((a: T, b: T) => a === b)): T[] {
    const copy = [...a].sort(compareFn);
    const ret = [copy[0]];
    for (let i = 1; i < copy.length; i++) {
        const elem = copy[i];
        if (ret.some((item) => eqFn(item, elem))) continue;
        ret.push(elem);
    }
    return ret;
}
