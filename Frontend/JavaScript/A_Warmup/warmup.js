"use strict";

/**
 * Складывает два целых числа
 * @param {Number} a Первое целое
 * @param {Number} b Второе целое
 * @throws {TypeError} Когда в аргументы переданы не числа
 * @returns {Number} Сумма аргументов
 */
function abProblem(a, b) {
  if (typeof a !== "number" || typeof b !== "number") {
    throw new TypeError();
  }
  return a + b;
}

/**
 * Определяет век по году
 * @param {Number} year Год, целое положительное число
 * @throws {TypeError} Когда в качестве года передано не число
 * @throws {RangeError} Когда год – отрицательное значение
 * @returns {Number} Век, полученный из года
 */
function centuryByYearProblem(year) {
  if (typeof year !== "number") {
    throw new TypeError();
  }
  if (year < 0) {
    throw new RangeError();
  }
  return Math.floor(year % 100 == 0 ? year / 100 : year / 100 + 1);
}

/**
 * Переводит цвет из формата HEX в формат RGB
 * @param {String} hexColor Цвет в формате HEX, например, '#FFFFFF'
 * @throws {TypeError} Когда цвет передан не строкой
 * @throws {RangeError} Когда значения цвета выходят за пределы допустимых
 * @returns {String} Цвет в формате RGB, например, '(255, 255, 255)'
 */
function colorsProblem(hexColor) {
  if (typeof hexColor !== "string" || !hexColor.startsWith("#")) {
    throw new TypeError();
  }
  if (!/#[0-9a-fA-F]{6}/.test(hexColor)) {
    throw new RangeError();
  }
  const r = parseInt(hexColor.substr(1, 2), 16);
  const g = parseInt(hexColor.substr(3, 2), 16);
  const b = parseInt(hexColor.substr(5, 2), 16);
  return `(${r}, ${g}, ${b})`;
}

/**
 * Находит n-ое число Фибоначчи
 * @param {Number} n Положение числа в ряде Фибоначчи
 * @throws {TypeError} Когда в качестве положения в ряде передано не число
 * @throws {RangeError} Когда положение в ряде не является целым положительным числом
 * @returns {Number} Число Фибоначчи, находящееся на n-ой позиции
 */
function fibonacciProblem(n) {
  // Ваше решение
  if (typeof n !== "number") {
    throw new TypeError();
  }
  if (n <= 0 || Math.floor(n) != n) {
    throw new RangeError();
  }
  let prev = 1;
  let cur = 1;
  for (let i = 2; i < n; i++) {
    const oldCur = cur;
    cur += prev;
    prev = oldCur;
  }
  return cur;
}

/**
 * Транспонирует матрицу
 * @param {(Any[])[]} matrix Матрица размерности MxN
 * @throws {TypeError} Когда в функцию передаётся не двумерный массив
 * @returns {(Any[])[]} Транспонированная матрица размера NxM
 */
function matrixProblem(matrix) {
  if (
    !Array.isArray(matrix) ||
    matrix.length == 0 ||
    !Array.isArray(matrix[0])
  ) {
    throw new TypeError();
  }
  const n = matrix.length;
  const m = matrix[0].length;
  if (m == 0) {
    return [[]];
  }
  const res = [];
  for (let i = 0; i < m; i++) {
    const row = [];
    for (let j = 0; j < n; j++) {
      row.push(matrix[j][i]);
    }
    res.push(row);
  }
  return res;
}

/**
 * Переводит число в другую систему счисления
 * @param {Number} n Число для перевода в другую систему счисления
 * @param {Number} targetNs Система счисления, в которую нужно перевести (Число от 2 до 36)
 * @throws {TypeError} Когда переданы аргументы некорректного типа
 * @throws {RangeError} Когда система счисления выходит за пределы значений [2, 36]
 * @returns {String} Число n в системе счисления targetNs
 */
function numberSystemProblem(n, targetNs) {
  if (typeof n !== "number" || typeof targetNs !== "number") {
    throw new TypeError();
  }
  if (targetNs < 2 || 36 < targetNs) {
    throw new RangeError();
  }
  return n.toString(targetNs);
}

/**
 * Проверяет соответствие телефонного номера формату
 * @param {String} phoneNumber Номер телефона в формате '8–800–xxx–xx–xx'
 * @throws {TypeError} Когда в качестве аргумента передаётся не строка
 * @returns {Boolean} Если соответствует формату, то true, а иначе false
 */
function phoneProblem(phoneNumber) {
  if (typeof phoneNumber !== "string") {
    throw new TypeError();
  }
  const pat = /^8-800-\d{3}-\d{2}-\d{2}$/;
  return pat.test(phoneNumber);
}

/**
 * Определяет количество улыбающихся смайликов в строке
 * @param {String} text Строка в которой производится поиск
 * @throws {TypeError} Когда в качестве аргумента передаётся не строка
 * @returns {Number} Количество улыбающихся смайликов в строке
 */
function smilesProblem(text) {
  if (typeof text !== "string") {
    throw new TypeError();
  }
  const a = ":-)";
  const b = "(-:";
  let res = 0;
  for (let i = 0; i < text.length - 2; i++) {
    if (text.substr(i, 3) === a || text.substr(i, 3) === b) {
      res++;
    }
  }
  return res;
}

/**
 * Определяет победителя в игре "Крестики-нолики"
 * Тестами гарантируются корректные аргументы.
 * @param {(('x' | 'o')[])[]} field Игровое поле 3x3 завершённой игры
 * @returns {'x' | 'o' | 'draw'} Результат игры
 */
function ticTacToeProblem(field) {
  for (let i = 0; i < 3; i++) {
    for (const p of ["x", "o"]) {
      let won = true;
      for (let j = 0; j < 3; j++) {
        if (field[i][j] !== p) {
          won = false;
          break;
        }
      }
      if (won) {
        return p;
      }
    }
  }
  for (let i = 0; i < 3; i++) {
    for (const p of ["x", "o"]) {
      let won = true;
      for (let j = 0; j < 3; j++) {
        if (field[j][i] !== p) {
          won = false;
          break;
        }
      }
      if (won) {
        return p;
      }
    }
  }
  for (const p of ["x", "o"]) {
    let won = true;
    for (let i = 0; i < 3; i++) {
      if (field[i][i] !== p) {
        won = false;
        break;
      }
    }
    if (won) {
      return p;
    }
  }
  for (const p of ["x", "o"]) {
    let won = true;
    for (let i = 0; i < 3; i++) {
      if (field[i][2 - i] !== p) {
        won = false;
        break;
      }
    }
    if (won) {
      return p;
    }
  }
  return "draw";
}

module.exports = {
  abProblem,
  centuryByYearProblem,
  colorsProblem,
  fibonacciProblem,
  matrixProblem,
  numberSystemProblem,
  phoneProblem,
  smilesProblem,
  ticTacToeProblem,
};
