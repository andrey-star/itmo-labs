let pigs;

let srcset;
let src;
let nextPig;

document.addEventListener('DOMContentLoaded', function() {
  srcset = 'assets/<name>.jpg 400w, assets/<name>-x2.jpg 800w';
  src = 'assets/<name>-x2.jpg';
  nextPig = 0;

  pigs = [
    {
      name: 'beth',
      age_months: 10
    },
    {
      name: 'caroline',
      age_months: 9
    },
    {
      name: 'diana',
      age_months: 13
    },
    {
      name: 'juna',
      age_months: 15
    },
    {
      name: 'kate',
      age_months: 6
    },
    {
      name: 'lisa',
      age_months: 2
    },
    {
      name: 'lucy',
      age_months: 7
    }
  ];

  onAction();
});

function onAction() {
  const pig = pigs[nextPig];
  animate(false);
  document.getElementById('card-img-source').srcset = srcset.replaceAll('<name>', pig.name);
  document.getElementById('card-img').src = src.replaceAll('<name>', pig.name);
  document.getElementById('card-name').innerHTML = capitalize(pig.name);
  document.getElementById('card-age').innerHTML = pig.age_months + ' months';
  nextPig++;
  nextPig %= pigs.length;
  animate(true);
}

function animate(animIn) {
  const elem = document.getElementById('card');
  let op = animIn ? 0 : 1;
  let step = 0.05;
  step = animIn ? step : -step;
  const id = setInterval(() => {
    if ((animIn && op >= 1) || (!animIn && op <= 0)) {
      clearInterval(id);
    } else {
      op += step;
      elem.style.opacity = op;
    }
  }, 10);
}

function capitalize(string) {
  return string.charAt(0).toUpperCase() + string.slice(1);
}
