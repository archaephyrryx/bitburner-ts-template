import { NS } from "@ns";

type SymbolData = { live: false } | { live: true, data: string[] };

export let Symbols: SymbolData = { live: false };

export function getSymbols(ns: NS): string[] {
    if (!Symbols.live) {
        Symbols = { live: true, data: ns.stock.getSymbols() };
    }
    return Symbols.data;
}

export type StockInfo = { principal: number, current: number };

export function stockValue(ns: NS, sym: string): StockInfo {
    const position = ns.stock.getPosition(sym);
    const principal = position[0] * position[1];
    const current = position[0] * ns.stock.getBidPrice(sym);
    return { principal, current };
}

export function addStockInfo(x: StockInfo, y: StockInfo): StockInfo {
    return {
        principal: x.principal + y.principal,
        current: x.current + y.current,
    }
}

export function aggregateStockInfo(ns: NS): StockInfo {
    if (ns.stock.hasTIXAPIAccess()) {
        return getSymbols(ns).reduce((acc, sym) => addStockInfo(acc, stockValue(ns, sym)), { principal: 0, current: 0 });
    } else {
        return { principal: 0, current: 0 };
    }
}
