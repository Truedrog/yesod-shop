import {FETCH_PRODUCTS_BEGIN, FETCH_PRODUCTS_SUCCESS, FETCH_PRODUCTS_FAILURE} from "./productActionTypes";

export function fetchProducts() {
    return dispatch => {
        dispatch(fetchProductsBegin());
        return fetch("/api/products")
            .then(response => response.json())
            .then(json => {
                dispatch(fetchProductsSuccess(json.result));
                return json.products;
            })
            .catch(error =>
                dispatch(fetchProductsFailure(error))
            );
    };
}


export const fetchProductsBegin = () => ({
    type: FETCH_PRODUCTS_BEGIN
});

export const fetchProductsSuccess = products => ({
    type: FETCH_PRODUCTS_SUCCESS,
    payload: { products }
});

export const fetchProductsFailure = error => ({
    type: FETCH_PRODUCTS_FAILURE,
    payload: { error }
});
