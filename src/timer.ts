import { NS } from "@ns";
import { ServicePort } from "./global";

let timerId = 0;

async function timer(ns: NS, time: number, label: string): Promise<void> {
    let ticks = 0;
    while (ticks < time) {
        ns.printf("%s: %s out of %s (%s done)", label, ns.tFormat(ticks), ns.tFormat(time), ns.formatPercent(ticks / time));
        await ns.sleep(1000);
        ticks += 1000;
    }
}

export async function main(ns: NS): Promise<void> {
    const timePort = ns.getPortHandle(ServicePort.HarvestTimer);
    const labelPort = ns.getPortHandle(ServicePort.HarvestTimerMessage);
    ns.disableLog("sleep");
    for (; ;) {
        if (timePort.empty()) {
            await ns.sleep(100);
            continue;
        }
        const timeData = timePort.read();
        const labelData = labelPort.empty() ? `Timer ${timerId++}` : labelPort.read();
        const time = timeData as number;
        const label = labelData as string;
        await timer(ns, time, label);
    }
}