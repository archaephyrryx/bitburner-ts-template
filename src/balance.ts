import { NS, MoneySources } from "@ns";

export async function main(ns: NS) {
    const sources = ns.getMoneySources();
    ns.tprint(`Since Last Install ($): ${ns.formatNumber(sources.sinceInstall.total)}`);
    const { total, augmentations, casino, corporation, crime, hacking, hacknet, hacknet_expenses, servers, sleeves, stock, work, infiltration, hospitalization } = sources.sinceInstall;
    showValuePercent(ns, "Augmentations", augmentations, total);
    showValuePercent(ns, "Casino", casino, total);
    showValuePercent(ns, "Corporation", corporation, total);
    showValuePercent(ns, "Crime", crime, total);
    showValuePercent(ns, "Hacking", hacking, total);
    showValuePercent(ns, "HackNet", hacknet + hacknet_expenses, total);
    showValuePercent(ns, "Hospitalization", hospitalization, total);
    showValuePercent(ns, "Infiltration", infiltration, total);
    showValuePercent(ns, "Servers", servers, total);
    showValuePercent(ns, "Sleeves", sleeves, total);
    showValuePercent(ns, "Stock", stock, total);
    showValuePercent(ns, "Work", work, total);
}

/** @param ns {NS} */
function showValuePercent(ns: NS, label: string, value: number, total: number) {
    if (total == 0) {
        return;
    }
    if (value != 0) {
        ns.tprint(`\t${label} ($): ${ns.formatNumber(value)} (${ns.formatPercent(value / total)})`);
    }
}
