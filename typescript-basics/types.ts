// Type defines: meaning of data, set of values, set of operations
// Type ensures data integrity and enforces data access
// Type defines immutability (const), encapsulation (private, scope)
// Type allows for composability, modularity, and documentation

const _findFirstNegativeNumber = (numbers: number[]): number | undefined => {
    for (const num of numbers) {
        if (num < 0) {
            return num;
        }
    }
};

const _findFirstOneCharString = (strings: string[]): string | undefined => {
    for (const str of strings) {
        if (str.length === 1) {
            return str;
        }
    }
};

const findFirst = <T>(items: T[], predicate: (item: T) => boolean): T | undefined => {
    for (const it of items) {
        if (predicate(it)) {
            return it;
        }
    }
};

const findFirstNegativeNumber = (numbers: number[]): number | undefined => {
    return findFirst(numbers, (num) => num < 0);
};

const findFirstOneCharString = (strings: string[]): string | undefined => {
    return findFirst(strings, (str) => str.length === 1);
};

// const numbers = [1, 2, -4];
// const negativeNumber = findFirstNegativeNumber(numbers);
// console.log(negativeNumber);

// const strings = ["ab", "cd", "e"];
// const oneCharString = findFirstOneCharString(strings);
// console.log(oneCharString);

// Static typing (TypeScript), dynamic/duck typing (JavaScript)
// Strong typing (===) + explicit coersion, weak typing (==) + imlicit coersion

interface Duck {
    quack(): void;
}

const quacker = (duck: Duck) => {
    duck.quack();
};

// quacker({quack: () => console.log("quack")});

// Roadmap
// Primitive types, type composition (structural programming)
// Function types (functional programming)
// Subtyping: inheritance, composition, misins (object-oriented programming)
// Generics: type variables (generic programming)

// never (empty type, no value): throw, while(true): funciton does not return at all

const throwError = (message: string): never => {
    throw new Error(message);
};

const infinitLoop = (): never => {
    while (true) {
        console.log("processing...");
    }
};

// void (unit type, single value): side effect: function does not return any meaningful
// value, but returns

const logMessage = (message: string): void => {
    console.log(message);
};

// boolean (two valuesa): conditional branching and looping, short-circuiting
