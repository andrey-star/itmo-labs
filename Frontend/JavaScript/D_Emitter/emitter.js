/**
 * Возвращает новый emitter
 * @returns {Object}
 */
function getEmitter() {
  const events = new Map();
  return {
    /**
     * Подписаться на событие
     * @param {String} event
     * @param {Object} context
     * @param {Function} handler
     */
    on: function (event, context, handler) {
      if (!events.has(event)) {
        events.set(event, []);
      }
      events.get(event).push({ context, handler });
      return this;
    },

    /**
     * Отписаться от события
     * @param {String} event
     * @param {Object} context
     */
    off: function (event, context) {
      const matchingEvents = [];
      if (events.has(event)) {
        for (const curEvent of events.keys()) {
          if (curEvent === event || curEvent.startsWith(`${event}.`)) {
            matchingEvents.push(curEvent);
          }
        }
      }
      for (const matchingEvent of matchingEvents) {
        events.set(
          matchingEvent,
          events.get(matchingEvent).filter((ev) => ev.context !== context)
        );
      }
      return this;
    },

    /**
     * Уведомить о событии
     * @param {String} event
     */
    emit: function (event) {
      const sp = event.split('.'); // [slide, funny]
      const eventsBubble = [];
      for (let i = 0; i < sp.length; i++) {
        eventsBubble.unshift(
          (eventsBubble[0] ? eventsBubble[0] + '.' : '') + sp[i]
        );
      }
      for (const event of eventsBubble) {
        if (events.has(event)) {
          for (const {context, handler} of events.get(event)) {
            handler.call(context);
          }
        }
      }
      return this;
    },

    /**
     * Подписаться на событие с ограничением по количеству полученных уведомлений
     * @star
     * @param {String} event
     * @param {Object} context
     * @param {Function} handler
     * @param {Number} times – сколько раз получить уведомление
     */
    several: function (event, context, handler, times) {
      console.info(event, context, handler, times);
    },

    /**
     * Подписаться на событие с ограничением по частоте получения уведомлений
     * @star
     * @param {String} event
     * @param {Object} context
     * @param {Function} handler
     * @param {Number} frequency – как часто уведомлять
     */
    through: function (event, context, handler, frequency) {
      console.info(event, context, handler, frequency);
    },
  };
}

module.exports = {
  getEmitter,
};
