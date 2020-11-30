'use strict';

const { runInThisContext } = require('vm');

/**
 * Итератор по друзьям
 * @constructor
 * @param {Object[]} friends
 * @param {Filter} filter
 */
function Iterator(friends, filter) {
  this.friends = friends;
  this.circles = this.init(friends, filter);
  this.curFriend = 0;
}
Iterator.prototype.init = function (friends, filter, maxLevel) {
  if (!(filter instanceof Filter)) {
    throw new TypeError();
  }
  let nextCircle = friends.filter((fr) => fr.best);
  const mark = new Set(nextCircle.map((fr) => fr.name));
  let circles = [];
  while (nextCircle.length > 0) {
    nextCircle.sort((f1, f2) => f1.name.localeCompare(f2.name));
    circles.push(nextCircle);
    nextCircle = this.nextCircle(circles[circles.length - 1], mark);
  }
  if (maxLevel !== undefined) {
    circles = circles.slice(0, Math.max(0, maxLevel));
  }
  return circles
    .map((circle) => circle.filter((fr) => filter.filter(fr)))
    .flat();
};
Iterator.prototype.nextCircle = function (curCircle, mark) {
  const nextCircle = [];
  for (const person of curCircle) {
    for (const friendName of person.friends) {
      if (!mark.has(friendName)) {
        nextCircle.push(this.friends.find((fr) => fr.name === friendName));
        mark.add(friendName);
      }
    }
  }
  return nextCircle;
};
Iterator.prototype.done = function () {
  return this.curFriend >= this.circles.length;
};
Iterator.prototype.next = function () {
  if (!this.done()) {
    return this.circles[this.curFriend++];
  }
  return null;
};

/**
 * Итератор по друзям с ограничением по кругу
 * @extends Iterator
 * @constructor
 * @param {Object[]} friends
 * @param {Filter} filter
 * @param {Number} maxLevel – максимальный круг друзей
 */
function LimitedIterator(friends, filter, maxLevel) {
  this.friends = friends;
  this.circles = this.init(friends, filter, maxLevel);
  this.curCircle = 0;
  this.curFriend = 0;
}
LimitedIterator.prototype = Object.create(Iterator.prototype);
LimitedIterator.prototype.constructor = LimitedIterator;

/**
 * Фильтр друзей
 * @constructor
 */
function Filter() {}
Filter.prototype.constructor = Filter;
Filter.prototype.filter = (_friend) => true;
/**
 * Фильтр друзей
 * @extends Filter
 * @constructor
 */
function MaleFilter() {}
MaleFilter.prototype = Object.create(Filter.prototype);
MaleFilter.prototype.constructor = MaleFilter;
MaleFilter.prototype.filter = (friend) => friend.gender === 'male';

/**
 * Фильтр друзей-девушек
 * @extends Filter
 * @constructor
 */
function FemaleFilter() {}
FemaleFilter.prototype = Object.create(Filter.prototype);
FemaleFilter.prototype.constructor = FemaleFilter;
FemaleFilter.prototype.filter = (friend) => friend.gender === 'female';

exports.Iterator = Iterator;
exports.LimitedIterator = LimitedIterator;

exports.Filter = Filter;
exports.MaleFilter = MaleFilter;
exports.FemaleFilter = FemaleFilter;
