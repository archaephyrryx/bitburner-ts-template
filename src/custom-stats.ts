import { NS } from "@ns";
import { explore } from "./graph";
import { StockInfo, aggregateStockInfo } from "./stock_helper";

/** @param {NS} ns **/
export async function main(ns: NS) {
    const args = ns.flags([["help", false]]);
    if (args.help) {
        ns.tprint("This script will enhance your HUD (Heads up Display) with custom statistics.");
        ns.tprint(`Usage: run ${ns.getScriptName()}`);
        ns.tprint("Example:");
        ns.tprint(`> run ${ns.getScriptName()}`);
        return;
    }

    const doc = document; // This is expensive! (25GB RAM) Perhaps there's a way around it? ;)
    const hook0 = doc.getElementById('overview-extra-hook-0');
    const hook1 = doc.getElementById('overview-extra-hook-1');
    // eslint-disable-next-line no-constant-condition
    while (true) {
        try {
            const headers = []
            const values = [];
            // Add script income per second
            headers.push("ScrInc");
            values.push(`$${ns.formatNumber(ns.getTotalScriptIncome()[0])}/sec`);
            // Add script exp gain rate per second
            headers.push("ScrExp");
            values.push(`${ns.formatNumber(ns.getTotalScriptExpGain())}/sec`);
            if (ns.stock.hasTIXAPIAccess()) {
                const investmentInfo = aggregateStockInfo(ns);
                if (investmentInfo.principal > 0) {
                    headers.push("ROI")
                    values.push(`$${ns.formatNumber(investmentInfo.principal)} => $${ns.formatNumber(investmentInfo.current)}`);
                }
            }
            // TODO: Add more neat stuff

            // Now drop it into the placeholder elements
            if (hook0 !== null) hook0.innerText = headers.join(" \n");
            if (hook1 !== null) hook1.innerText = values.join("\n");
        } catch (err) { // This might come in handy later
            ns.print("ERROR: Update Skipped: " + String(err));
        }
        await ns.sleep(1000);
    }
}
