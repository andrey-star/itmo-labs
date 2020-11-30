'use strict';

/**
 * Телефонная книга
 */
const phoneBook = new Map();
let query;
let END = '\0';
let index = 0;
let curCmdIndex = -1;
let curCmdStartIndex = 0;
let creates = 0;
let deletes = 0;
let adds = 0;
// let shows = 0;
let queryResults = [];

/**
 * Выполнение запроса на языке pbQL
 * @param {string} query
 * @returns {string[]} - строки с результатами запроса
 */
function run(queryy) {
  phoneBook.clear();
  queryResults = [];
  index = 0;
  curCmdIndex = -1;
  curCmdStartIndex = 0;
  query = queryy + END;
  command();
  return queryResults;
}

function command() {
  while (!test(END)) {
    curCmdStartIndex = index;
    curCmdIndex++;
    const cmd = parseIdentifier(notSpaceSemi);
    if (!cmds[cmd]) {
      index -= cmd.length;
      error(`Unknown command: '${cmd}'`);
    }
    if (!testNext(' ')) {
      error('Expected space');
    }
    cmds[cmd]();
  }
}

function escapeRegExp(string) {
  return string.replace(/[.*+?^${}()|[\]\\]/g, '\\$&'); // $& means the whole matched string
}

function createOrDeleteCommand(toDelete) {
  const arg = parseIdentifier(notSpaceSemi);
  if (!toDelete && arg !== 'контакт') {
    index -= arg.length;
    error("Expected 'контакт'");
  }
  if (
    toDelete &&
    arg !== 'контакт' &&
    arg !== 'телефон' &&
    arg !== 'почту' &&
    arg !== 'контакты,'
  ) {
    index -= arg.length;
    error("Expected 'контакт|контакты,|телефон|почту'");
  }
  if (arg === 'телефон' || arg === 'почту') {
    index -= arg.length;
    addOrDeleteInfo(true);
  } else if (arg === 'контакты,') {
    if (!testNext(' ')) {
      error("Expected space after 'контакты,'");
    }
    const q = parseQuery();
    if (!testNext(';')) {
      error('Expected ; after query');
    }
    deleteByQuery(q);
  } else if (arg === 'контакт') {
    if (!testNext(' ')) {
      error("Expected space after 'контакт'");
    }
    const name = parseIdentifier(notSemi);
    if (!testNext(';')) {
      error("Expected ';' after name");
    }
    if (name.length > 0) {
      if (toDelete) {
        phoneBook.delete(name);
      } else {
        const user = {
          phones: new Set(),
          emails: new Set(),
        };
        if (!phoneBook.has()) {
          phoneBook.set(name, user);
        }
      }
    }
  }
}

function deleteByQuery(q) {
  if (q !== '') {
    const toDelete = new Set();
    phoneBook.forEach((user, name) => {
      let match = matches(name, user, q);
      if (match) {
        toDelete.add(name);
      }
    });
    for (const name of toDelete) {
      phoneBook.delete(name);
    }
  }
}

function parseQuery() {
  let key = parseIdentifier(notSpaceSemi);
  if (key !== 'где') {
    index -= key.length;
    error("Expected 'где'");
  }
  if (!testNext(' ')) {
    error("Expected space after 'где'");
  }
  key = parseIdentifier(notSpaceSemi);
  if (key !== 'есть') {
    index -= key.length;
    error("Expected 'есть'");
  }
  if (!testNext(' ')) {
    error("Expected space after 'есть'");
  }
  let q = parseIdentifier(notSemi);
  return q;
}

function addOrDeleteInfo(toDelete, phones = new Set(), emails = new Set()) {
  let next;
  while (true) {
    const type = parseIdentifier(notSpaceSemi);
    if (type !== 'телефон' && type !== 'почту') {
      index -= type.length;
      error("Expected 'телефон|почту'");
    }
    if (!testNext(' ')) {
      error('Expected space after phone/number');
    }
    const arg = parseIdentifier(notSpaceSemi);
    if (type === 'телефон' && !arg.match(/^\d{10}$/)) {
      index -= arg.length;
      error("Expected '\\d{10}'");
    }
    if (type === 'почту' && arg.length === 0) {
      error('Expected non empty email');
    }
    if (type === 'телефон') {
      phones.add(arg);
    } else {
      emails.add(arg);
    }
    if (!testNext(' ')) {
      error('Expected space after phone/number');
    }
    next = parseIdentifier(notSpaceSemi);
    if (next !== 'и' && next !== 'для') {
      index -= next.length;
      error("Expected 'и|для'");
    }
    if (next === 'и') {
      if (!testNext(' ')) {
        error("Expected space after 'и'");
      }
    } else {
      break;
    }
  }
  if (next === 'для') {
    if (!testNext(' ')) {
      error("Expected space after 'для'");
    }
    next = parseIdentifier(notSpaceSemi);
    if (next !== 'контакта') {
      index -= next.length;
      error("Expected 'контакта'");
    }
    if (!testNext(' ')) {
      error("Expected space after 'контакта'");
    }
    const name = parseIdentifier(notSemi);
    if (name.length === 0) {
      error("Expected name after 'контакта'");
    }
    if (!testNext(';')) {
      error("Expected ';' after name");
    }
    const user = phoneBook.get(name);
    if (user) {
      for (const phone of phones) {
        if (toDelete) {
          user.phones.delete(phone);
        } else {
          user.phones.add(phone);
        }
      }
      for (const email of emails) {
        if (toDelete) {
          user.emails.delete(email);
        } else {
          user.emails.add(email);
        }
      }
    }
  }
}

function showCommand2(types = []) {
  let next;
  while (true) {
    const type = parseIdentifier(notSpaceSemi);
    if (type !== 'имя' && type !== 'почты' && type !== 'телефоны') {
      index -= type.length;
      error("Expected 'имя|почты|телефоны'");
    }
    if (!testNext(' ')) {
      error("Expected space after 'имя|почты|телефоны'");
    }
    types.push(type);
    next = parseIdentifier(notSpaceSemi);
    if (next !== 'и' && next !== 'для') {
      index -= next.length;
      error("Expected 'и|для'");
    }
    if (next === 'и') {
      if (!testNext(' ')) {
        error("Expected space after 'и'");
      }
    } else {
      break;
    }
  }

  if (next === 'для') {
    if (!testNext(' ')) {
      error("Expected space after 'для'");
    }
    next = parseIdentifier(notSpaceSemi);
    if (next !== 'контактов,') {
      index -= next.length;
      error("Expected 'контактов,'");
    }
    if (!testNext(' ')) {
      error("Expected space after 'контакты,'");
    }
    const q = parseQuery();
    if (!testNext(';')) {
      error('Expected ; after query');
    }
    if (q !== '') {
      phoneBook.forEach((user, name) => {
        let match = matches(name, user, q);
        if (match) {
          queryResults.push(toString(name, user, types));
        }
      });
    }
  }
}

function matches(name, user, q) {
  q = escapeRegExp(q);
  if (name.match(q)) {
    return true;
  }
  for (const phone of user.phones) {
    if (phone.match(q)) {
      return true;
    }
  }
  for (const email of user.emails) {
    if (email.match(q)) {
      return true;
    }
  }
  return false;
}

function toString(name, user, types) {
  const phones = [...user.phones]
    .map((phone) => {
      const first = phone.slice(0, 3);
      const second = phone.slice(3, 6);
      const third = phone.slice(6, 8);
      const fourth = phone.slice(8, 10);
      return `+7 (${first}) ${second}-${third}-${fourth}`;
    })
    .join(',');
  const emails = [...user.emails].join(',');
  let res = [];
  for (const type of types) {
    if (type === 'имя') {
      res.push(name);
    } else if (type === 'телефоны') {
      res.push(phones);
    } else if (type === 'почты') {
      res.push(emails);
    }
  }
  return res.join(';');
}

function parseIdentifier(possible) {
  let id = '';
  if (possible(getChar())) {
    id += getChar();
    while (possible(nextChar())) {
      id += getChar();
    }
  }
  return id;
}

function createCommand() {
  createOrDeleteCommand(false);
  creates++;
}

function deleteCommand() {
  createOrDeleteCommand(true);
  deletes++;
}

function addCommand() {
  addOrDeleteInfo(false);
  adds++;
}

function showCommand() {
  showCommand2();
}

/**
 * Вызывайте эту функцию, если есть синтаксическая ошибка в запросе
 * @param {number} cmdNumber – номер команды с ошибкой
 * @param {number} charNumber – номер символа, с которого запрос стал ошибочным
 */
function syntaxError(cmdNumber, charNumber) {
  throw new SyntaxError(
    `SyntaxError: Unexpected token at ${cmdNumber + 1}:${charNumber + 1}`
  );
}

function error(message) {
  // print(message);
  // die();
  syntaxError(curCmdIndex, index - curCmdStartIndex);
}

function die() {
  for (let i = 0; i < 1e11; i++) {}
}

function print(message) {
  console.log(
    `'${query}'. ${message} at ${curCmdIndex + 1}:${
      index - curCmdStartIndex + 1
    }`
  );
}

const cmds = {
  Создай: createCommand,
  Удали: deleteCommand,
  Добавь: addCommand,
  Покажи: showCommand,
};

function test(c) {
  return query[index] === c;
}

function testNext(c) {
  const testRes = test(c);
  if (testRes) {
    index++;
  }
  return testRes;
}

function getChar() {
  return query[index];
}

function nextChar() {
  return getChar(++index);
}

function notSpaceSemi(c) {
  return c !== ' ' && notSemi(c);
}

function notSemi(c) {
  return c !== ';' && c !== END;
}

module.exports = { phoneBook, run };
