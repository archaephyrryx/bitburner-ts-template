import { NS, MoneySources } from "@ns";

export async function main(ns: NS) {
    const sources = ns.getMoneySources();

    ns.tprint(`Since Last Install ($): ${ns.formatNumber(sources.sinceInstall.total)}`);
    const { augmentations, casino, corporation, crime, hacking, hacknet, hacknet_expenses, servers, sleeves, stock, work, infiltration, hospitalization } = sources.sinceInstall;
    if (augmentations != 0) ns.tprint(`\tAugmentations ($): ${ns.formatNumber(augmentations)}`);
    if (casino != 0) ns.tprint(`\tCasino ($): ${ns.formatNumber(casino)}`);
    if (corporation != 0) ns.tprint(`\tCorporation ($): ${ns.formatNumber(corporation)}`);
    if (crime != 0) ns.tprint(`\tCrime ($): ${ns.formatNumber(crime)}`);
    if (hacking != 0) ns.tprint(`\tHacking ($): ${ns.formatNumber(hacking)}`);
    if (hacknet != 0) ns.tprint(`\tHackNet ($): ${ns.formatNumber(hacknet + hacknet_expenses)}`);
    if (hospitalization != 0) ns.tprint(`\tHospitalization ($): ${ns.formatNumber(hospitalization)}`);
    if (infiltration != 0) ns.tprint(`\tInfiltration: ${ns.formatNumber(infiltration)}`);
    if (servers != 0) ns.tprint(`\tServer Cost ($): ${ns.formatNumber(servers)}`);
    if (sleeves != 0) ns.tprint(`\tSleeves ($): ${ns.formatNumber(sleeves)}`);
    if (stock != 0) ns.tprint(`\tStock ($): ${ns.formatNumber(stock)}`);
    if (work != 0) ns.tprint(`\tWork ($): ${ns.formatNumber(work)}`);
}
