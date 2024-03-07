import { NS } from '@ns';
import { CityName, Cities } from './global';
import { CHAOS_LIMIT } from './blade';

export async function main(ns: NS) {
    if (!ns.bladeburner.inBladeburner()) ns.exit();
    bladeburnerTravel(ns);
}

export function bladeburnerTravel(ns: NS) {
    const currentCity = ns.bladeburner.getCity();
    const citiesChaos = getCitiesChaos(ns);
    const chaosRank = citiesChaos.findIndex(([city,]) => city == currentCity);

    if (chaosRank < 0) {
        throw new Error(`Current city ${currentCity} not in list returned by getCitiesChaos`);
    } else {
        const currentChaos = citiesChaos[chaosRank][1];
        const [paxCity, minChaos] = citiesChaos[0]
        if (currentChaos > CHAOS_LIMIT && minChaos <= CHAOS_LIMIT) {
            ns.toast(`Switching Bladeburner operations to ${paxCity} (Chaos: ${ns.formatNumber(currentChaos, 3)} => ${ns.formatNumber(minChaos, 3)})`, "success", 5000);
            ns.bladeburner.switchCity(citiesChaos[0][0]);
        }
    }
}
function getCitiesChaos(ns: NS): [`${CityName}`, number][] {
    const ret: [`${CityName}`, number][] = [];
    for (const city of Cities) {
        const chaos = ns.bladeburner.getCityChaos(city);
        ret.push([city, chaos]);
    }
    return ret.toSorted(([, chaosA], [, chaosB]) => chaosA - chaosB);
}
