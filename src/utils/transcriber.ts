import axios from 'axios';
// eslint-disable-next-line @typescript-eslint/no-var-requires
const geoip = require('geoip-lite');

interface Currency {
    code: string;
    currency: {
        symbol: string,
    };
    language: {
        code: string,
    };
}

// @ts-expect-error It will assign replace_fr on string. It doesn't matter if it doesn't exist already.
String.prototype.replace_fr = function (target: string, replacement: string): string {
    const pattern = new RegExp(`\\b${target}\\b(?=(?:(?:[^"]*"){2})*[^"]*$)`, 'g');
    
    return this.replace(pattern, replacement);
}

const rightsideCurrencies = [
    "€", // Euro
    "£", // British Pound
    "CHF", // Swiss Franc
    "kr", // Danish Krone, Norwegian Krone, Swedish Krona
    "zł", // Polish Zloty
    "Ft", // Hungarian Forint
    "Kč", // Czech Koruna
    "kn", // Croatian Kuna
    "RSD", // Serbian Dinar
    "лв", // Bulgarian Lev
    "lei", // Romanian Leu
    "₽", // Russian Ruble
    "₺", // Turkish Lira
    "₴" // Ukrainian Hryvnia
];   

// @ts-expect-error It will assign replace_fr on string. It doesn't matter if it doesn't exist already.
String.prototype.replace_currency = function (currency: string): string {
    const pattern = new RegExp(`${rightsideCurrencies.includes(currency) ? "{}" + currency : currency + "{}"}`, 'g');

    return this.replace(pattern, "${}");
}

export async function get_currency(currencies: Currency[]) {
    const { country } = await get_country();
    const currency = currencies.find((el: Currency) => el.code === country)

    return currency.currency.symbol;
}

async function get_country() {
    const response = await axios.get('https://api64.ipify.org?format=json');
    const ip = response.data.ip;
    const geo = await geoip.lookup(ip);

    return geo;
}
// Nlang transcribe
export function transcribe(code: string, currency: string) {
    return code
        // @ts-expect-error replace_fr is assigned earlier in the code.
        .replace_fr(";", '!')
        .replace_fr("rn", ';')
        .replace_fr("equal", '=')
        .replace_fr("lvar", 'let')
        .replace_fr("cvar", 'const')
        .replace_fr("out", 'println')
        .replace_fr("sus", 'if')
        .replace_fr("nil", 'null')
        .replace_fr("otherwise", 'else')
        .replace_fr("nahqual", '!=')
        .replace_fr("real", '==')
        .replace_fr("btw", '&&')
        .replace_fr("or", '|')
        .replace_fr("def", 'fn')
        .replace_fr("ghost", 'math')
        .replace_fr("fs", 'fs')
        .replace_fr("for", 'for')
        .replace_fr("smlt", '<')
        .replace_fr("grt", '>')
        .replace_fr("true", 'true')
        .replace_fr("false", 'false')
        .replace_fr("attempt", 'try')
        .replace_fr("rescue", 'catch')
        .replace_fr("exec", 'exec')
        .replace_fr("input", 'input')
        .replace_fr("minus", "-")
        .replace_fr("plus", "+")
        .replace_fr("dicri", "--")
        .replace_fr("incri", "++")
        .replace_fr("mul", "*")
        .replace_fr("div", "/")
        .replace_fr("escape", "exit")
        .replace_fr("setend", "setTimeout")
        .replace_fr("setint", "setInterval")
        .replace_fr("hash_it", "hash")
        .replace_fr("encrypt_it", "encrypt")
        .replace_fr("decrypt_it", "decrypt")
        .replace(/: number/g, '')
        .replace(/: string/g, '')
        .replace(/: object/g, '')
        .replace(/: boolean/g, '')
        .replace_currency(currency);
}