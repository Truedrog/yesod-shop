import {FETCH_PRODUCTS_BEGIN, FETCH_PRODUCTS_FAILURE, FETCH_PRODUCTS_SUCCESS} from "./productActionTypes";
import {combineReducers} from "redux";

const initialState = {
    loading: false,
    error: null
};

const products = (state, action) => {
    switch (action.type) {
        default:
            return state
    }
}

const status = (state = initialState, action) => {
    switch (action.type) {
        case FETCH_PRODUCTS_BEGIN:
            return {
                ...state,
                error: null,
                loading: true
            };
        case FETCH_PRODUCTS_SUCCESS:
            return {
                ...state,
                loading: false,
            };
        case FETCH_PRODUCTS_FAILURE:
            return {
                ...state,
                loading: false,
                error: action.payload.error,
            };
        default:
            return state
    }
}

const byId = (state = {}, action) => {
    switch (action.type) {
        case FETCH_PRODUCTS_SUCCESS:
            return {
                ...state,
                ...action.payload.products.reduce((obj, product) => {
                    obj[product.id] = product
                    return obj
                }, {})
            }
        case FETCH_PRODUCTS_BEGIN:
            return {
                ...state
            }
        case FETCH_PRODUCTS_FAILURE:
            return {
                ...state,
            }
        default:
            const {productId} = action
            if (productId) {
                return {
                    ...state,
                    [productId]: products(state[productId], action)
                }
            }
            return state
    }
}

const visibleIds = (state = [], action) => {
    switch (action.type) {
        case FETCH_PRODUCTS_BEGIN:
        case FETCH_PRODUCTS_FAILURE:
            // Mark the state as "loading" so we can show a spinner or something
            // Also, reset any errors. We're starting fresh.
            return [];
        case FETCH_PRODUCTS_SUCCESS:
            return action.payload.products.map(product => product.id);
        default:
            return state
    }
}

export default combineReducers({
    byId,
    visibleIds,
    status
})

export const getProduct = (state, id) =>
    state.byId[id];

export const getVisibleProducts = state =>
    state.visibleIds.map(id => getProduct(state, id));

export const getStatus = state =>
    state.status;
