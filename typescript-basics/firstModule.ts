export function sum(...numbers: number[]) {
    return numbers.reduce((sum, value) => sum + value, 0);
}
