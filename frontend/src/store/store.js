// third-party
import { createStore, applyMiddleware, compose } from 'redux';
import thunk from 'redux-thunk';
import logger from 'redux-logger'

// reducer
import rootReducer from './rootReducer';


function load() {
    let state;

    try {
        state = localStorage.getItem('state');

        if (typeof state === 'string') {
            state = JSON.parse(state);
        }
    } catch (error) {
        // eslint-disable-next-line no-console
        console.error(error);
    }

    return state || undefined;
}

const store = createStore(rootReducer, load(), compose(
    applyMiddleware(thunk),
    // process.env.NODE_ENV === 'development' && window.__REDUX_DEVTOOLS_EXTENSION__ && window.__REDUX_DEVTOOLS_EXTENSION__({trace: true, traceLimit: 25})
));

function save() {
    try {
        localStorage.setItem('state', JSON.stringify(store.getState()));
    } catch (error) {
        // eslint-disable-next-line no-console
        console.error(error);
    }
}

store.subscribe(() => save());

export default store;
