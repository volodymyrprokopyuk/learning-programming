import {sum as sumNumbers} from "./firstModule";

test("Test sumNumbers on several input arguments", () => {
    const result = sumNumbers(1, 2, 3, 4, 5, 6, 7);
    expect(result).toBe(28);
});

describe("Function sumNumbers", () => {
    it("Should add two numbers", () => {
        const result = sumNumbers(1, 2);
        expect(result).toBe(3);
    });
});
