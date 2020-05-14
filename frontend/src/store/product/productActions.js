import {createAction} from 'redux-act';

export function fetchProducts(sliceName = "", category = "") {
    let str = category ? `/${category}` : "";

    switch (sliceName) {
        case "A":
            return performFetch(str, beginA, successA, failureA);
        case "B":
            return performFetch(str, beginB, successB, failureB);
        default:
            return performFetch(str, begin, success, failure);
    }
}

const performFetch = (str, begin, success, failure) => dispatch => {
    dispatch(begin());
    return fetch(`/api/products${str}`)
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
