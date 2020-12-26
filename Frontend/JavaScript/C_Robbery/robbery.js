'use strict';

function getTimeUTC(time) {
  const tz = parseInt(time.split('+')[1]);
  const dhm = time.split('+')[0].split(' ');
  let day = 0;
  if (dhm.length == 2) {
    switch (dhm[0]) {
      case 'ПН':
        day = 0;
        break;
      case 'ВТ':
        day = 1;
        break;
      case 'СР':
        day = 2;
        break;
      case 'ЧТ':
        day = 3;
        break;
      case 'ПТ':
        day = 4;
        break;
      case 'СБ':
        day = 5;
        break;
      case 'ВС':
        day = 6;
        break;
      default:
        throw new Error('Assertion Error in day of week');
    }
  }

  let hm = dhm[dhm.length - 1].split(':');
  const h = parseInt(hm[0]);
  const m = parseInt(hm[1]);
  return (24 * day + h - tz) * 60 + m;
}

function getPeriodUTC(period) {
  return {
    from: getTimeUTC(period.from),
    to: getTimeUTC(period.to),
  };
}

function fromMinutes(mins) {
  const m = mins % 60;
  mins -= m;
  const h = (mins / 60) % 24;
  mins -= 60 * h;
  const d = mins / 60 / 24;
  let day;
  switch (d) {
    case 0:
      day = 'ПН';
      break;
    case 1:
      day = 'ВТ';
      break;
    case 2:
      day = 'СР';
      break;
    case 3:
      day = 'ЧТ';
      break;
    case 4:
      day = 'ПТ';
      break;
    case 5:
      day = 'СБ';
      break;
    case 6:
      day = 'ВС';
      break;
    case 7:
      day = 'ПН2';
      break;
    default:
      throw new Error('Assertion Error in day of week');
  }
  return {
    day,
    h,
    m,
  };
}

function momentToString({ day, h, m }) {
  return `${day} ${h
    .toString()
    .padStart(2, '0')
    .slice(-2)}:${m.toString().padStart(2, '0').slice(-2)}`;
}

/**
 * @param {Object} schedule Расписание Банды
 * @param {number} duration Время на ограбление в минутах
 * @param {Object} workingHours Время работы банка
 * @param {string} workingHours.from Время открытия, например, "10:00+5"
 * @param {string} workingHours.to Время закрытия, например, "18:00+5"
 * @returns {Object}
 */
function getAppropriateMoment(schedule, duration, workingHours) {
  const bankTz = parseInt(workingHours.from.split('+')[1]);
  const events = [];
  for (const name in schedule) {
    for (const period of schedule[name]) {
      const utc = getPeriodUTC(period);
      events.push({ m: utc.from, type: 'from', id: name });
      events.push({ m: utc.to, type: 'to', id: name });
    }
  }
  const startEnd = getPeriodUTC({
    to: 'ПН 00:00+' + bankTz,
    from: 'СР 23:59+' + bankTz,
  });
  events.push({ m: startEnd.from, type: 'from', id: 'bank' });
  events.push({ m: startEnd.to, type: 'to', id: 'bank' });

  const bankT = getPeriodUTC(workingHours);
  for (let i = 0; i <= 6; i++) {
    events.push({ m: 24 * 60 * (i - 1) + bankT.to, type: 'from', id: 'bank' });
    events.push({ m: 24 * 60 * i + bankT.from, type: 'to', id: 'bank' });
  }

  events.sort((a, b) => {
    const diff = a.m - b.m;
    if (diff != 0) {
      return a.m - b.m;
    }
    if (a.type === 'to') {
      return -1;
    }
    return 1;
  });
  // console.log(events);

  let busy = 1;
  let freeStart = 0;
  const freePeriods = [];
  for (const event of events) {
    if (event.type === 'to') {
      busy--;
      if (busy === 0) {
        freeStart = event.m;
      }
    }
    if (event.type === 'from') {
      if (busy === 0 && freeStart != event.m) {
        freePeriods.push({ from: freeStart, to: event.m });
      }
      busy++;
    }
  }
  const av = freePeriods.filter(
    (period) => period.to - period.from >= duration
  );
  let curPeriod;
  return {
    /**
     * Найдено ли время
     * @returns {boolean}
     */
    exists() {
      if (curPeriod === undefined) {
        curPeriod = av[0];
      }
      return curPeriod !== undefined && curPeriod !== null;
    },

    /**
     * Возвращает отформатированную строку с часами
     * для ограбления во временной зоне банка
     *
     * @param {string} template
     * @returns {string}
     *
     * @example
     * ```js
     * getAppropriateMoment(...).format('Начинаем в %HH:%MM (%DD)') // => Начинаем в 14:59 (СР)
     * ```
     */
    format(template) {
      if (curPeriod === undefined || curPeriod === null) {
        return '';
      }
      const from = fromMinutes(curPeriod.from + 60 * bankTz);
      template = template.replace('%DD', from.day);
      const hPad = from.h.toString().padStart(2, '0').slice(-2);
      const mPad = from.m.toString().padStart(2, '0').slice(-2);
      template = template.replace('%HH', hPad);
      template = template.replace('%MM', mPad);
      return template;
    },

    /**
     * Попробовать найти часы для ограбления позже [*]
     * @note Не забудь при реализации выставить флаг `isExtraTaskSolved`
     * @returns {boolean}
     */
    tryLater() {
      if (av.length == 0 || curPeriod == null) {
        return false;
      }
      if (curPeriod === undefined) {
        curPeriod = av[0];
      }
      const start = curPeriod.from + 30;
      for (const period of av) {
        // console.log(period.to - Math.max(start, period.from));
        if (period.to - Math.max(start, period.from) >= duration) {
          curPeriod = { from: Math.max(start, period.from), to: period.to };
          return true;
        }
      }
      return false;
    },
  };
}

const isExtraTaskSolved = true;

module.exports = {
  getAppropriateMoment,
  isExtraTaskSolved,
};
