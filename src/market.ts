import { NS } from '@ns';

let globalStocks: string[];
let totalBudget = 0;
const expectedCommission = 100000;

type CycleStats = { purchased: string[], sold: string[], ignored: string[], maintained: string[] };
const emptyCycleStats: () => CycleStats = () => { return { purchased: [], sold: [], ignored: [], maintained: [] } };

function printStockInfo(ns: NS, stock: string) {
    const currentBidPrice = ns.stock.getBidPrice(stock);
    const currentPosition = ns.stock.getPosition(stock);
    const currentOwned = currentPosition[0];
    if (currentOwned < 1) {
        return;
    }
    ns.printf("Stock Information for %s", stock);
    const currentAveragePrice = currentPosition[1];
    const currentTotalValue = currentOwned * currentBidPrice;
    const currentTotalCost = currentOwned * currentAveragePrice;
    const currentProfit = currentTotalValue - currentTotalCost;
    const currentForecast = ns.stock.getForecast(stock);
    const currentProfitPercentage = currentProfit / currentTotalCost;
    ns.printf(">%s", stock);
    ns.print("  Owned: " + ns.formatNumber(currentOwned, 3, 1000, true));
    ns.print("  Average Price: $" + ns.formatNumber(currentAveragePrice, 3, 1000, false));
    ns.printf("  Forecast: %0.02f", currentForecast);
    ns.print("  Total Value: " + ns.formatNumber(currentTotalValue, 3, 1000, true));
    ns.print("  Profit: " + ns.formatNumber(currentProfit));
    ns.print("  Profit Percentage: " + ns.formatPercent(currentProfitPercentage, 2));
}

export function increaseBudget(ns: NS, fraction: string | number | boolean): boolean {
    if (Number.isNaN(fraction)) {
        ns.tprint("ERROR: increaseBudget requires a fraction argument!");
        return false;
    }
    if (typeof fraction === "string") {
        fraction = Number(fraction);
    } else if (typeof fraction === "boolean") {
        ns.tprint("ERROR: increaseBudget requires a fraction argument!");
        return false;
    }
    if (fraction > 1 || fraction <= 0) {
        ns.tprintf("Invalid fraction: %d", fraction);
        return false;
    }
    const totalMoney = ns.getServerMoneyAvailable("home");
    const moneyToIncrease = totalMoney * fraction;
    totalBudget += moneyToIncrease;
    return true;
}

export function buyStock(ns: NS, stock: string, quantity: number): void {
    if (isNaN(quantity)) {
        ns.tprint("ERROR: buy requires a quantity argument!");
        return;
    }
    if (quantity < 1) {
        ns.tprint("ERROR: buy requires a positive quantity argument!");
        return;
    }
    globalStocks = globalStocks ?? ns.stock.getSymbols();
    if (globalStocks.indexOf(stock) === -1) {
        ns.tprint("ERROR: buy requires a valid stock argument!");
        return;
    }
    const currentAskPrice = ns.stock.getAskPrice(stock);
    const requiredBudget = currentAskPrice * quantity + 100000;
    if (requiredBudget > totalBudget) {
        ns.tprint("ERROR: buy requires more budget!");
        ns.tprint("HINT: You must have at least %d to buy %d of %s", requiredBudget, quantity, stock);
        ns.tprint("HINT: Your budget is %d and your total money is %d", totalBudget, ns.getServerMoneyAvailable("home"));
        return;
    }
    const boughtPrice = ns.stock.buyStock(stock, quantity);
    if (boughtPrice == 0) {
        ns.tprint("ERROR: buy failed!");
        return;
    }
    ns.tprint("Buying %d of %s at %d", quantity, stock, boughtPrice);
    totalBudget -= boughtPrice * quantity;
    return;
}

export function sellStock(ns: NS, stock: string, amount: string | number) {
    const stocks = globalStocks ?? ns.stock.getSymbols();
    if (stock === "*") {
        for (let i = 0; i < stocks.length; i++) {
            sellStock(ns, stocks[i], amount);
        }
    } else {
        if (stocks.indexOf(stock) === -1) {
            ns.tprint("ERROR: sell requires a valid stock argument!");
            return;
        }
        let amountToSell = 0;
        if (amount === "all") {
            amountToSell = ns.stock.getPosition(stock)[0];
        } else if (amount === "half") {
            amountToSell = Math.floor(ns.stock.getPosition(stock)[0] / 2);
        } else if (Number.isSafeInteger(amount)) {
            amountToSell = Number(amount);
        } else {
            ns.tprint("ERROR: sell requires a valid amount argument!");
            return;
        }
        if (amountToSell < 1) {
            ns.tprint("ERROR: sell requires a positive amount argument!");
            return;
        }
        const currentBidPrice = ns.stock.getBidPrice(stock);
        const originalAskPrice = ns.stock.getPosition(stock)[1];
        const historicalValue = originalAskPrice * amountToSell;
        const moneyToReceive = currentBidPrice * amountToSell - expectedCommission;

        if (moneyToReceive < 0) {
            ns.tprint("ERROR: " + stock + " is not worth enough to sell!");
            const breakingPoint = (expectedCommission + originalAskPrice) / amountToSell;
            ns.tprint("HINT: In order to break even, value must rise to %f per stock", breakingPoint);
            return;
        } else if (moneyToReceive < historicalValue) {
            ns.tprint("WARNING: " + stock + " is not worth as much as you paid!");
            ns.tprint("HINT: You will lose %f per stock", (historicalValue - moneyToReceive) / amountToSell);
            ns.tprint("HINT: Proceeding with trade anyway... (edit script if you wish to change this behavior)");
        }
        const soldPrice = ns.stock.sellStock(stock, amountToSell);
        ns.tprint("Successfully sold %d of %s at %d", amountToSell, stock, soldPrice);
        ns.tprint("You made %d", moneyToReceive - historicalValue);
        totalBudget += soldPrice * amountToSell;
        return;
    }
}

export async function watchStock(ns: NS, stock: string, iterations?: number): Promise<void> {
    if (stock === "*") {
        let i: number = iterations ?? Infinity;
        while (i > 0) {
            for (const stock of globalStocks) {
                await watchStock(ns, stock, 1);
            }
            i--;
            if (i > 0) {
                await ns.sleep(16000);
            }
        }
    } else {
        if (globalStocks.indexOf(stock) === -1) {
            ns.tprint("ERROR: could not find symbol " + stock);
            return;
        }
        let i: number = iterations ?? Infinity;
        do {
            const currentBidPrice = ns.stock.getBidPrice(stock);
            const currentAskPrice = ns.stock.getAskPrice(stock);
            const currentPosition = ns.stock.getPosition(stock);
            const currentOwned = currentPosition[0];
            if (currentOwned < 1) {
                ns.printf("Not watching %s because you don't own any", stock);
                return;
            }
            const currentAveragePrice = currentPosition[1];
            const currentTotalValue = currentOwned * currentBidPrice;
            const currentTotalCost = currentOwned * currentAveragePrice;
            const currentProfit = currentTotalValue - currentTotalCost;
            const currentProfitPerStock = currentProfit / currentOwned;
            const currentProfitPercentage = currentProfit / currentTotalCost;
            ns.tprint("=====================================");
            ns.tprintf("Watching %s", stock);
            ns.tprintf("Bid: %d", currentBidPrice);
            ns.tprintf("Ask: %d", currentAskPrice);
            ns.tprintf("Owned: %d", currentOwned);
            ns.tprintf("Average Price: %d", currentAveragePrice);
            ns.tprintf("Total Value: %d", currentTotalValue);
            ns.tprintf("Total Cost: %d", currentTotalCost);
            ns.tprintf("Profit: %d", currentProfit);
            ns.tprintf("Profit Per Stock: %d", currentProfitPerStock);
            ns.tprintf("Profit Percentage: %0.02f", currentProfitPercentage);
            i--;
            if (i > 0) {
                ns.tprint("Waiting 16 seconds...");
                await ns.sleep(16000);
            }
        } while (i > 0);
    }
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

export async function autoTrader(ns: NS, canBuy = true) {
    ns.tprint("Starting autoTrader...");
    totalBudget = (totalBudget == 0) ? ns.getServerMoneyAvailable("home") / 2 : totalBudget;
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
    for (; ; cycle++) {
        ns.printf("====== AutoTrader cycle %d ======", cycle);
        const moneyAvailable = ns.getServerMoneyAvailable("home");
        const oldBudget = totalBudget;
        totalBudget = Math.max(moneyAvailable / 4, Math.min(totalBudget, moneyAvailable));
        ns.printf("Budget: %d ==> %d (%0.02f%% ==> %0.02f%% of available money)", oldBudget, totalBudget, oldBudget / moneyAvailable * 100, totalBudget / moneyAvailable * 100);
        const budgetPerStock = Math.max(totalBudget, 0) / (1 + (portfolioSize <= 0 ? 1 : portfolioSize));
        const cycleStats = emptyCycleStats();
        const randomOrderStocks = randomizedStockOrder(ns);
        let remainingSymbolPurchases = Math.min(randomOrderStocks.length, Math.floor(portfolioSize * 1.5 + 1));
        inner: for (const stock of randomOrderStocks) {
            const currentBidPrice = ns.stock.getBidPrice(stock);
            const currentAskPrice = ns.stock.getAskPrice(stock);
            const currentPosition = ns.stock.getPosition(stock);
            const currentOwned = currentPosition[0];
            const currentForecast = ns.stock.getForecast(stock);
            const originalValue = currentOwned * currentBidPrice;
            const grossPrice = currentOwned * currentAskPrice;
            const netPrice = grossPrice - expectedCommission;
            const netProfit = netPrice - originalValue;
            const netProfitPercentage = netProfit / originalValue;
            if (currentForecast >= 0.66 && canBuy) {
                if (remainingSymbolPurchases <= 0) {
                    ns.printf("INFO: Not buying %s because we have no more purchases left this cycle", stock);
                }
                const effectiveSymbols = Math.min(remainingSymbolPurchases, (currentForecast >= 0.8) ? 3 : (currentForecast >= 0.7) ? 2 : 1);
                const idealValue = budgetPerStock * effectiveSymbols;
                const realValue = Math.min(idealValue, moneyAvailable);
                const maxBuy = ns.stock.getMaxShares(stock) - currentOwned;
                const amountToBuy = Math.min(Math.floor(realValue / currentAskPrice), maxBuy);
                if (amountToBuy > 0) {
                    const buyPrice = ns.stock.buyStock(stock, amountToBuy);
                    if (buyPrice > 0) {
                        ns.printf("Bought %s of %s at $%s", ns.formatNumber(amountToBuy, 3, 1000, true), stock, ns.formatNumber(buyPrice, 4, 1000, false));
                        remainingSymbolPurchases -= effectiveSymbols;
                        cycleStats.purchased.push(stock);
                    }
                    totalBudget -= buyPrice * amountToBuy;
                    if (totalBudget < 0) {
                        ns.toast("WARNING: Current stock budget is negative. This happens when a stock is bought at high value due to favorable forecast.", "warning", 5000);
                        ns.print("INFO: New stocks will not be bought until budget is manually funded, or a profitable stock is sold.");
                    }
                } else {
                    ns.printf("INFO: Stock %s is forecast to increase in value (%0.02f), but autotrader does not have enough budget to purchase stocks.", stock, currentForecast);
                    continue inner;
                }
            } else if (currentForecast <= 0.5) {
                if (currentOwned > 0) {
                    if (netProfitPercentage >= 0) {
                        const sellPrice = ns.stock.sellStock(stock, currentOwned);
                        ns.printf("Sold %s of %s at $%s", ns.formatNumber(currentOwned, 3, 1000, true), stock, ns.formatNumber(sellPrice, 4, 1000, false));
                        totalBudget += sellPrice * currentOwned;
                        cycleStats.sold.push(stock);
                    } else {
                        ns.printf("INFO: Stock %s is forecast to decline in value (%0.02f), but autotrader will not sell it unless it is profitable to do so.", stock, currentForecast);
                        cycleStats.maintained.push(stock);
                        continue inner;
                    }
                } else {
                    cycleStats.ignored.push(stock);
                }
            } else {
                ns.printf("INFO: Stock %s is forecast to remain stable (%0.02f), so no action will be taken.", stock, currentForecast);
                if (currentOwned > 0) {
                    cycleStats.maintained.push(stock);
                } else {
                    cycleStats.ignored.push(stock);
                }
                continue inner;
            }
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
        await ns.sleep(8000);
    }
}


export async function main(ns: NS): Promise<void> {
    ns.tprint("Starting automated trade script...");
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
    switch (ns.args[0]) {
        case "getBudget":
            ns.tprintf("Current budget: %d", totalBudget);
            return;
        case "increaseBudget":
            if (ns.args.length < 2) {
                ns.tprint("ERROR: increaseBudget requires a fraction argument!");
                return;
            }
            if (!increaseBudget(ns, ns.args[2])) {
                ns.tprint("ERROR: increaseBudget failed!");
                return;
            }
            return;
        case "buy":
            if (ns.args.length < 3) {
                ns.tprint("ERROR: buy requires a stock and quantity argument!");
                return;
            }
            buyStock(ns, ns.args[1] as string, Number(ns.args[2]));
            return;
        case "sell":
            if (ns.args.length < 3) {
                ns.tprint("ERROR: sell requires a stock and quantity argument!");
                return;
            }
            sellStock(ns, ns.args[1] as string, ns.args[2] as number | string);
            return;
        case "watch":
            if (ns.args.length < 2) {
                ns.tprint("WARN: watch requires a stock argument! (assuming *)");
            }
            await watchStock(ns, ns.args[1] as string);
            return;
        case "autotrade":
            await autoTrader(ns);
            return;
        case "autosell":
            await autoTrader(ns, false);
            return;
        case "report":
            await watchStock(ns, (ns.args[1] ?? "*") as string, 1);
            return;
        default:
            ns.tprint("ERROR: Invalid argument!");
            ns.tprint("HINT: Commands are increaseBudget, buy, sell, watch, autotrade, and report.");
            return;
    }
}