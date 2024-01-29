import { NS } from '@ns';
import { aggregateStockInfo } from './stock_helper';

const expectedCommission = 100000;

export let globalStocks: string[];

export const WaterMarks = {
    forceBuy: 0.90,
    tripleSymbol: 0.80,
    overAllocate: 0.75,
    doubleSymbol: 0.70,
    purchaseAbove: 0.60,
    sellBelow: 0.50,
};


type CycleStats = { purchased: string[], sold: string[], ignored: string[], maintained: string[] };
const emptyCycleStats: () => CycleStats = () => { return { purchased: [], sold: [], ignored: [], maintained: [] } };

function printStockInfo(ns: NS, stock: string) {
    const currentBidPrice = ns.stock.getBidPrice(stock);
    const currentPosition = ns.stock.getPosition(stock);
    const currentOwned = currentPosition[0];
    if (currentOwned < 1) {
        return;
    }
    const currentAveragePrice = currentPosition[1];
    const currentTotalValue = currentOwned * currentBidPrice;
    const currentTotalCost = currentOwned * currentAveragePrice;
    const currentProfit = currentTotalValue - currentTotalCost - expectedCommission;
    const currentForecast = ns.stock.getForecast(stock);
    const currentProfitPercentage = currentProfit / currentTotalCost;
    ns.print(`>>${stock}: ${ns.formatNumber(currentOwned, 3, 1000, true)} shares (max: ${ns.formatNumber(ns.stock.getMaxShares(stock), 3, 1000, true)}) @${ns.formatNumber(currentForecast, 2)}`);

    ns.print(`  Total Value: ${ns.formatNumber(currentTotalValue, 3, 1000, true)}\t|\tProfit: ${ns.formatNumber(currentProfit)} (${ns.formatPercent(currentProfitPercentage, 2)})`);
}

export function randomizedStockOrder(ns: NS): string[] {
    const stocks = ns.stock.getSymbols();
    const randomizedStocks = [];
    while (stocks.length > 0) {
        const randomIndex = Math.floor(Math.random() * stocks.length);
        randomizedStocks.push(stocks[randomIndex]);
        stocks.splice(randomIndex, 1);
    }
    return randomizedStocks;
}

function getBudget(ns: NS, liquidity = 0.10): number {
    const liquid = ns.getServerMoneyAvailable("home");
    const stockInfo = aggregateStockInfo(ns);
    const principalValue = stockInfo.principal;
    const currentValue = stockInfo.current;
    const totalPrincipal = liquid + principalValue;
    const totalCurrent = liquid + currentValue;

    const totalGM = Math.sqrt(totalPrincipal * totalCurrent);
    return (liquid - liquidity * totalGM);
}

export async function autoTrader(ns: NS, canBuy = true, forceSell = false) {
    const origCanBuy = canBuy;
    ns.tprint("Starting autoTrader...");
    if (!ns.stock.hasWSEAccount()) {
        ns.tprint("ERROR: You do not have a WSE account!");
        return;
    }
    if (!ns.stock.hasTIXAPIAccess()) {
        ns.tprint("ERROR: You do not have TIX API access!");
        return;
    }
    if (!ns.stock.has4SDataTIXAPI()) {
        ns.tprint("WARNING: You do not have 4S Data TIX API access!");
    }
    globalStocks = globalStocks ?? ns.stock.getSymbols();
    let portfolioSize = globalStocks.length;
    let cycle = 0;
    let totalBudget = getBudget(ns);
    if (forceSell) {
        const randomOrderStocks = randomizedStockOrder(ns);
        for (const stock of randomOrderStocks) {
            const currentBidPrice = ns.stock.getBidPrice(stock);

            const currentPosition = ns.stock.getPosition(stock);
            const currentOwned = currentPosition[0];
            const currentAveragePrice = currentPosition[1];

            const principal = currentOwned * currentAveragePrice;
            const grossYield = currentOwned * currentBidPrice;
            const netYield = grossYield - expectedCommission;
            const netProfit = netYield - principal;
            const netProfitPercentage = netProfit / principal;

            if (currentOwned > 0) {
                if (netProfitPercentage >= 0) {
                    const sellPrice = ns.stock.sellStock(stock, currentOwned);
                    ns.printf("Sold %s of %s at $%s", ns.formatNumber(currentOwned, 3, 1000, true), stock, ns.formatNumber(sellPrice, 4, 1000, false));
                    ns.toast(`Made ${ns.formatNumber(netProfit, 4, 1000, true)} (${ns.formatPercent(netProfitPercentage)}) on ${stock} sale`, "success", 5000);
                } else {
                    const sellPrice = ns.stock.sellStock(stock, currentOwned);
                    ns.printf("Sold %s of %s at $%s", ns.formatNumber(currentOwned, 3, 1000, true), stock, ns.formatNumber(sellPrice, 4, 1000, false));
                    ns.toast(`Lost ${ns.formatNumber(netProfit, 4, 1000, true)} (${ns.formatPercent(netProfitPercentage)}) on ${stock} sale`, "info", 5000);
                }
            }
        }
        return;
    }
    for (; ; cycle++) {
        ns.printf("====== AutoTrader cycle %d ======", cycle);
        const moneyAvailable = ns.getServerMoneyAvailable("home");
        const oldBudget = totalBudget;
        totalBudget = getBudget(ns);
        ns.printf("Budget: %d ==> %d (%0.02f%% ==> %0.02f%% of available money)", oldBudget, totalBudget, oldBudget / moneyAvailable * 100, totalBudget / moneyAvailable * 100);
        if (totalBudget <= 0) {
            canBuy = false;
        } else {
            canBuy = origCanBuy;
        }
        const budgetPerStock = Math.max(totalBudget, 0) / (1 + (portfolioSize <= 0 ? 1 : portfolioSize));
        const cycleStats = emptyCycleStats();
        const randomOrderStocks = randomizedStockOrder(ns);
        let remainingSymbolPurchases = Math.min(randomOrderStocks.length, Math.floor(portfolioSize * 1.5 + 1));
        for (const stock of randomOrderStocks) {
            const currentBidPrice = ns.stock.getBidPrice(stock);
            const currentAskPrice = ns.stock.getAskPrice(stock);

            const currentPosition = ns.stock.getPosition(stock);
            const currentOwned = currentPosition[0];
            const currentAveragePrice = currentPosition[1];
            const currentForecast = ns.stock.getForecast(stock);

            const principal = currentOwned * currentAveragePrice;
            const grossYield = currentOwned * currentBidPrice;
            const netYield = grossYield - expectedCommission;
            const netProfit = netYield - principal;
            const netProfitPercentage = netProfit / principal;

            let bought = false;
            let sold = false;
            const oldCanBuy = canBuy;
            if (currentForecast >= WaterMarks.forceBuy) {
                canBuy = true;
            }
            if (currentForecast >= WaterMarks.overAllocate) {
                remainingSymbolPurchases += 1;
            }
            if (currentForecast >= WaterMarks.purchaseAbove) {
                if (remainingSymbolPurchases <= 0) {
                    ns.printf("INFO: Not buying %s because we have no more purchases left this cycle", stock);
                } else if (currentOwned >= ns.stock.getMaxShares(stock)) {
                    ns.printf("INFO: Cannot buy any more stocks of %s", stock);
                } else if (!canBuy) {
                    ns.printf("INFO: AutoTrader has buying disabled, otherwise would buy %s", stock);
                } else {
                    const effectiveSymbols = Math.min(remainingSymbolPurchases, (currentForecast >= WaterMarks.tripleSymbol) ? 3 : (currentForecast >= WaterMarks.doubleSymbol) ? 2 : 1);
                    const idealValue = budgetPerStock * effectiveSymbols;
                    const realValue = Math.min(idealValue, moneyAvailable);
                    const maxBuy = ns.stock.getMaxShares(stock) - currentOwned;
                    const amountToBuy = Math.min(Math.floor(realValue / currentAskPrice), maxBuy);
                    if (amountToBuy > 0) {
                        if (amountToBuy * currentAskPrice * 0.1 < expectedCommission) {
                            ns.printf("INFO: Not buying %s because the commission would be more than 10%% of the purchase price", stock);
                        } else {
                            const buyPrice = ns.stock.buyStock(stock, amountToBuy);
                            if (buyPrice > 0) {
                                ns.printf("Bought %s of %s at $%s", ns.formatNumber(amountToBuy, 3, 1000, true), stock, ns.formatNumber(buyPrice, 4, 1000, false));
                                bought = true;
                                remainingSymbolPurchases -= effectiveSymbols;
                                cycleStats.purchased.push(stock);
                            }
                        }
                    } else {
                        ns.printf("INFO: Stock %s is forecast to increase in value (%0.02f), but autotrader does not have enough budget to purchase stocks.", stock, currentForecast);
                    }
                }
            } else if (currentForecast <= WaterMarks.sellBelow) {
                if (currentOwned > 0) {
                    if (netProfitPercentage >= 0) {
                        const sellPrice = ns.stock.sellStock(stock, currentOwned);
                        ns.printf("Sold %s of %s at $%s", ns.formatNumber(currentOwned, 3, 1000, true), stock, ns.formatNumber(sellPrice, 4, 1000, false));
                        ns.toast(`Made ${ns.formatNumber(netProfit, 4, 1000, true)} (${ns.formatPercent(netProfitPercentage)}) on ${stock} sale`, "success", 5000);
                        cycleStats.sold.push(stock);
                        sold = true;
                    } else {
                        const sellPrice = ns.stock.sellStock(stock, currentOwned);
                        ns.printf("Sold %s of %s at $%s", ns.formatNumber(currentOwned, 3, 1000, true), stock, ns.formatNumber(sellPrice, 4, 1000, false));
                        ns.toast(`Lost ${ns.formatNumber(netProfit, 4, 1000, true)} (${ns.formatPercent(netProfitPercentage)}) on ${stock} sale`, "info", 5000);
                        cycleStats.sold.push(stock);
                        sold = true;
                    }
                }
            } else {
                ns.printf("INFO: Stock %s is forecast to remain stable (%0.02f), so no action will be taken.", stock, currentForecast);
            }
            if (!bought && !sold) {
                if (currentOwned > 0) {
                    cycleStats.maintained.push(stock);
                } else {
                    cycleStats.ignored.push(stock);
                }
            }
            canBuy = oldCanBuy;
        }

        ns.printf("====== AutoTrader cycle %d stats ======", cycle);
        ns.printf("Purchased: %s", cycleStats.purchased.join(", "));
        ns.printf("Sold: %s", cycleStats.sold.join(", "));
        ns.printf("Maintained: %s", cycleStats.maintained.join(", "));

        cycleStats.purchased.sort();
        for (const symbol of cycleStats.purchased) {
            printStockInfo(ns, symbol);
        }
        cycleStats.maintained.sort();
        for (const symbol of cycleStats.maintained) {
            printStockInfo(ns, symbol);
        }

        ns.printf("====== AutoTrader cycle %d complete ======", cycle);
        portfolioSize = cycleStats.purchased.length + cycleStats.maintained.length;
        await ns.sleep(6000);
    }
}


export async function main(ns: NS): Promise<void> {
    ns.tprint("Starting automated trade script...");
    globalStocks = globalStocks ?? ns.stock.getSymbols();
    switch (ns.args[0]) {
        case "autotrade":
            ns.tail();
            await autoTrader(ns);
            return;
        case "autosell":
            ns.tail();
            await autoTrader(ns, false, (ns.args[1] == "--force"));
            return;
        default:
            ns.tprint("ERROR: Invalid argument!");
            ns.tprint("HINT: Commands are autotrade and autosell.");
            return;
    }
}
