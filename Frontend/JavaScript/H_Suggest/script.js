    function debounce(func, wait, immediate) {
    let timeout;
    return function () {
        let context = this,
        args = arguments;
        let later = function () {
        timeout = null;
        if (!immediate) {
            func.apply(context, args);
        }
        };
        let callNow = immediate && !timeout;
        clearTimeout(timeout);
        timeout = setTimeout(later, wait);
        if (callNow) {
        func.apply(context, args);
        }
    };
    }

    function proxy(url) {
    return 'https://cors-anywhere.herokuapp.com/' + url;
    }

    function updateSugs(query) {
    if (query === '') {
        setSugs('', []);
    } else {
        const url = `http://autocomplete.travelpayouts.com/places2?locale=en&max=5&term=${query}&types[]=city&types[]=airport`;
        fetch(proxy(url))
        .then((res) => res.json())
        .then((sugs) =>
            sugs.map((sug) => {
            return {
                name: sug.name,
                code: sug.code,
                country: sug.country_name,
            };
            })
        )
        .then((sugs) => setSugs(query, sugs))
        .catch((error) => console.error(`API error occured: ${error}`));
    }
    }

    function setSugs(query, sugs) {
    removeChildren(searchSugs);
    for (let i = 0; i < sugs.length; i++) {
        const sug = sugs[i];
        const text = `${sug.name} ${sug.code}, ${sug.country}`;
        const searchSug = document.createElement('button');
        searchSug.id = 'search-sug-' + i;
        searchSug.classList.add('search-sug');
        searchSug.append(text);
        searchSug.addEventListener('click', (e) => {
        e.preventDefault();
        curSug = i;
        searchInput.value = text;
        });
        searchSugs.appendChild(searchSug);
    }
    curSug = -1;
    totalSugs = sugs.length;
    curQuery = query;
    }

    function removeChildren(element) {
    while (element.firstChild) {
        element.removeChild(element.lastChild);
    }
    }

    function onKeyDown(event) {
    if (event.key === 'ArrowDown' && curSug < totalSugs - 1) {
        curSug++;
        const el = document.getElementById(`search-sug-${curSug}`);
        el.focus();
        searchInput.value = el.textContent;
    } else if (event.key === 'ArrowUp' && curSug > 0) {
        curSug--;
        const el = document.getElementById(`search-sug-${curSug}`);
        el.focus();
        searchInput.value = el.textContent;
    } else if (event.key === 'ArrowUp' && curSug == 0) {
        if (curQuery) {
        searchInput.value = curQuery;
        }
        curSug--;
        searchInput.focus();
    }
    }

    const searchSugs = document.getElementById('search-sugs');
    const searchInput = document.getElementById('search-input');
    const searchForm = document.getElementById('search-form');
    let curSug, totalSugs, curQuery;

    document.addEventListener('DOMContentLoaded', () => {
    const debouncedUpdate = debounce(updateSugs, 300, false);
    searchInput.addEventListener('input', (e) => {
        if (e.target.value === '') {
        setSugs('', []);
        return;
        }
        debouncedUpdate(e.target.value);
    });
    searchInput.addEventListener('click', (e) => {
        curSug = -1;
    });
    searchForm.addEventListener('keydown', onKeyDown);
    });
