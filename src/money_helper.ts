import { NS } from "@ns";
import { aggregateStockInfo } from "./stock_helper";

export function canAfford(ns: NS, amount: number): [boolean, boolean] {
    const liquid = ns.getServerMoneyAvailable("home");
    const total = aggregateStockInfo(ns).current + liquid;
    return [liquid >= amount, total >= amount];
}
