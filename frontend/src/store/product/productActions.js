import {createAction} from 'redux-act';

export function fetchProducts(sliceName = "", category = "", options = {}) {

    let str = category ? `/${category}` : "";

    let actions;
    switch (sliceName) {
        case "A":
            actions = {begin:beginA, success: successA, failure:failureA}
            return performFetch(str, actions, options);
        case "B":
            actions = {begin:beginB, success: successB,failure: failureB}
            return performFetch(str, actions, options);
        default:
            actions = {begin, success, failure}
            return performFetch(str, actions, options);
    }
}

const performFetch = (str, actions, options) => dispatch => {
    const {begin, success, failure} = actions;
    const {limit, offset, sort} = options;
    let query = new URLSearchParams();
    query.append("limit", limit ?? 0);
    query.append("offset", offset ?? 0);
    query.append("sort", sort);
    dispatch(begin());
    return fetch(`/api/products${str}?${query.toString()}`)
        .then(response => response.json())
        .then(json => {
            dispatch(success(json.result));
            return json.result;
        })
        .catch(error =>
            dispatch(failure(error))
        );
};

export const fetchProductsBegin = name => createAction(`FETCH_PRODUCTS_BEGIN_${name}`);
export const fetchProductsSuccess = name => createAction(`FETCH_PRODUCTS_SUCCESS_${name}`);
export const fetchProductsFailure = name => createAction(`FETCH_PRODUCTS_FAILURE_${name}`);

export const begin = createAction(`FETCH_PRODUCTS_BEGIN`);
export const success = createAction(`FETCH_PRODUCTS_SUCCESS`);
export const failure = createAction(`FETCH_PRODUCTS_FAILURE`);
export const beginA = fetchProductsBegin("A");
export const successA = fetchProductsSuccess("A");
export const failureA = fetchProductsFailure("A");
export const beginB = fetchProductsBegin("B");
export const successB = fetchProductsSuccess("B");
export const failureB = fetchProductsFailure("B");
