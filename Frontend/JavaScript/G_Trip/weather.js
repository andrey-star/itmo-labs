"use strict";

const fetch = require("node-fetch");

const API_KEY = require("./key.json");

/**
 * @typedef {object} TripItem Город, который является частью маршрута.
 * @property {number} geoid Идентификатор города
 * @property {number} day Порядковое число дня маршрута
 */

class TripBuilder {
  constructor(geoids) {
    this.geoids = geoids;
    this.plan = [];
    this.limit = 7;
  }
  /**
   * Метод, добавляющий условие наличия в маршруте
   * указанного количества солнечных дней
   * Согласно API Яндекс.Погоды, к солнечным дням
   * можно приравнять следующие значения `condition`:
   * * `clear`;
   * * `partly-cloudy`.
   * @param {number} daysCount количество дней
   * @returns {object} Объект планировщика маршрута
   */
  sunny(daysCount) {
    for (let i = 0; i < daysCount; i++) {
      this.plan.push("sunny");
    }
    return this;
  }

  /**
   * Метод, добавляющий условие наличия в маршруте
   * указанного количества пасмурных дней
   * Согласно API Яндекс.Погоды, к солнечным дням
   * можно приравнять следующие значения `condition`:
   * * `cloudy`;
   * * `overcast`.
   * @param {number} daysCount количество дней
   * @returns {object} Объект планировщика маршрута
   */
  cloudy(daysCount) {
    for (let i = 0; i < daysCount; i++) {
      this.plan.push("cloudy");
    }
    return this;
  }

  /**
   * Метод, добавляющий условие максимального количества дней.
   * @param {number} daysCount количество дней
   * @returns {object} Объект планировщика маршрута
   */
  max(daysCount) {
    this.limit = daysCount;
    return this;
  }

  /**
   * Метод, возвращающий Promise с планируемым маршрутом.
   * @returns {Promise<TripItem[]>} Список городов маршрута
   */
  build() {
    this.limit = Math.min(this.limit, this.plan.length);
    return Promise.all(
      this.geoids.map((geoid) =>
        getWeatherForecast(geoid, this.limit)
          .then((res) => res.json())
          .then((json) => {
            return {
              geoid: json.info.geoid,
              conds: json.forecasts
                .map((forecast) => forecast.parts.day_short.condition)
                .map((cond) => {
                  if (cond === "clear" || cond === "partly-cloudy") {
                    return "sunny";
                  } else if (cond === "cloudy" || cond === "overcast") {
                    return "cloudy";
                  }
                }),
            };
          })
      )
    ).then((cs) => {
      const cityDays = new Map();
      const plan = this.plan;
      const limit = this.limit;

      function getTrip(prevTrip, prevGeoid) {
        if (prevTrip.length === plan.length) {
          return prevTrip;
        }
        for (const c of cs) {
          let d = cityDays.get(c.geoid);
          if (d === undefined) {
            d = 0;
          }
          if (
            (d !== 0 && !(d < limit && c.geoid === prevGeoid)) ||
            c.conds[prevTrip.length] !== plan[prevTrip.length]
          ) {
            continue;
          }
          cityDays.set(c.geoid, d + 1);
          const nextTrip = getTrip(
            prevTrip = [...prevTrip, { geoid: c.geoid, day: prevTrip.length + 1 }],
            c.geoid
          );
          if (nextTrip) {
            return nextTrip;
          }
          cityDays.set(c.geoid, d);
        }
        return null;
      }
      const res = getTrip([], -1);
      if (!res) {
        throw Error("Не могу построить маршрут!");
      }
      return res;
    });
  }
}

function getWeatherForecast(geoid, limit) {
  return fetch(
    `https://api.weather.yandex.ru/v2/forecast?limit=${limit}&geoid=${geoid}&hours=false`
  );
}

function getWeatherForecast_API_KEY(geoid, limit) {
  return fetch(`https://api.weather.yandex.ru/v2/forecast?limit=7&geoid=${geoid}&hours=false`, {
    method: "GET",
    headers: { "X-Yandex-API-Key": API_KEY },
  });
}

/**
 * Фабрика для получения планировщика маршрута.
 * Принимает на вход список идентификаторов городов, а
 * возвращает планировщик маршрута по данным городам.
 *
 * @param {number[]} geoids Список идентификаторов городов
 * @returns {TripBuilder} Объект планировщика маршрута
 * @see https://yandex.ru/dev/xml/doc/dg/reference/regions-docpage/
 */
function planTrip(geoids) {
  return new TripBuilder(geoids);
}

module.exports = {
  planTrip,
};
