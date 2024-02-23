import { CrimeType, NS } from '@ns';
import { CityName, SleeveTask } from './global';

export const fallbackCrime = "Shoplift";

// export const fallbackAction: SleeveTask = { type: "CLASS", location: "Rothman University", classType: "Computer Science" };
// export const fallbackAction: Partial<SleeveTask> = { type: "CRIME", crimeType: "Bond Forgery" };
export const fallbackAction: Partial<SleeveTask> & { crimeType: `${CrimeType}` } = { type: "CRIME", crimeType: "Rob Store" };

export function moveToCity(ns: NS, sleeve: number, dest: CityName): boolean {
    if (ns.sleeve.getSleeve(sleeve).city == dest) {
        return true;
    } else {
        return ns.sleeve.travel(sleeve, dest);
    }
}
