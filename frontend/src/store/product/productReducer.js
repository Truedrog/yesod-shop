import {combineReducers} from "redux";
import {createReducer} from 'redux-act';

const initialState = {
    loading: false,
    error: null
};

export const status = name => createReducer({
    [name ? `FETCH_PRODUCTS_BEGIN_${name}` : "FETCH_PRODUCTS_BEGIN"]: (state) => ({
        ...state,
        error: null,
        loading: true
    }),
    [name ? `FETCH_PRODUCTS_SUCCESS_${name}` : "FETCH_PRODUCTS_SUCCESS"]: (state) => ({
        ...state,
        error: null,
        loading: false
    }),
    [name ? `FETCH_PRODUCTS_FAILURE_${name}` : "FETCH_PRODUCTS_FAILURE"]: (state, payload) => ({
        ...state,
        loading: false,
        error: payload.error,
    })
}, initialState);

export const byId = name => createReducer({
    [name ? `FETCH_PRODUCTS_BEGIN_${name}` : "FETCH_PRODUCTS_BEGIN"]: (state) => ({...state}),
    [name ? `FETCH_PRODUCTS_SUCCESS_${name}` : "FETCH_PRODUCTS_SUCCESS"]: (state, payload) => ({
            ...state, ...payload.reduce((obj, product) => {
                obj[product.id] = product
                return obj;
            }, {})
        }
    ),
    [name ? `FETCH_PRODUCTS_FAILURE_${name}` : "FETCH_PRODUCTS_FAILURE"]: (state) => ({...state})
}, {});

export const visibleIds = name => createReducer({
    [name ? `FETCH_PRODUCTS_BEGIN_${name}` : "FETCH_PRODUCTS_BEGIN"]: (_) => [],
    [name ? `FETCH_PRODUCTS_SUCCESS_${name}` : "FETCH_PRODUCTS_SUCCESS"]: (state, payload) => payload.map(product => product.id),
    [name ? `FETCH_PRODUCTS_FAILURE_${name}` : "FETCH_PRODUCTS_FAILURE"]: (_) => []
}, []);

export default (id) => combineReducers({
    [`byId${id ? id : ""}`]: byId(id),
    [`visibleIds${id ? id : ""}`]: visibleIds(id),
    [`status${id ? id : ""}`]: status(id)
})

export const getProduct = (state, id, sliceName = "") => state[`byId${sliceName}`][id];

export const getVisibleProducts = (state, sliceName= "") => {
    return state[`visibleIds${sliceName}`].map(id => getProduct(state, id, sliceName));
}

export const getStatus = (state, sliceName = "") => state[`status${sliceName}`];
