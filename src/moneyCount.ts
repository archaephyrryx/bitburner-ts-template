import { NS } from "@ns";
import { Symbols, aggregateStockInfo } from './stock_helper';

function displayMoney(ns: NS) {
    const liquidMoney = ns.getServerMoneyAvailable("home");
    ns.tprint(`Liquid Money: $${ns.formatNumber(liquidMoney)}`);
    const investmentInfo = aggregateStockInfo(ns);
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
