import { NS, AutocompleteData } from "@ns";

export async function main(ns: NS) {
    const mode = ns.args[0] ?? "crime";

    switch (mode) {
        case "work":
            if (ns.exec("sleeve-man.company.js", "home") === 0) {
                ns.tprint("WARNING: Unable to execute 'sleeve-man.company.js'");
                ns.exit();
            }
            break;
        case "faction":
            if (ns.exec("sleeve-man.faction.js", "home") === 0) {
                ns.tprint("WARNING: Unable to execute 'sleeve-man.faction.js'");
                ns.exit();
            }
            break;
        case "crime":
        default:
            if (ns.exec("sleeve-man.crime.js", "home") === 0) {
                ns.tprint("WARNING: Unable to execute 'sleeve-man.crime.js'");
                ns.exit();
            }
            break;
    }
}

// eslint-disable-next-line @typescript-eslint/no-unused-vars
export function autocomplete(data: AutocompleteData, args: string[]) {
    return ["work", "crime", "faction"];
}
