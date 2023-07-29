import { NS } from "@ns";

type StockInfo = { principal: number, current: number };

function stockValue(ns: NS, sym: string): StockInfo {
    const position = ns.stock.getPosition(sym);
    const principal = position[0] * position[1];
    const current = position[0] * ns.stock.getBidPrice(sym);
    return { principal, current };
}

function addStockInfo(x: StockInfo, y: StockInfo): StockInfo {
    return {
        principal: x.principal + y.principal,
        current: x.current + y.current,
    }
}

function displayMoney(ns: NS) {
    const liquidMoney = ns.getServerMoneyAvailable("home");
    ns.tprint(`Liquid Money: $${ns.formatNumber(liquidMoney)}`);
    const investmentInfo = ns.stock.getSymbols().reduce((acc, sym) => addStockInfo(acc, stockValue(ns, sym)), { principal: 0, current: 0 });
    if (investmentInfo.principal === 0) {
        return;
    }
    const returnOnInvestment = (investmentInfo.current / investmentInfo.principal) - 1;
    const totalMoney = liquidMoney + investmentInfo.current;
    ns.tprint(`Investment: $${ns.formatNumber(investmentInfo.current)} (${ns.formatPercent(returnOnInvestment)})`);
    ns.tprint(`Total: $${ns.formatNumber(totalMoney)}`);
}

export async function main(ns: NS): Promise<void> {
    displayMoney(ns);
    return;
}