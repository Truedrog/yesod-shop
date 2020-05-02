import {FETCH_CATEGORIES_BEGIN, FETCH_CATEGORIES_SUCCESS, FETCH_CATEGORIES_FAILURE} from "./categoryActionTypes";

export function fetchCategories() {
    return dispatch => {
        dispatch(fetchCategoriesBegin());
        return fetch(`/api/products`)
            .then(response => response.json())
            .then(json => {
                dispatch(fetchCategoriesSuccess(json.result));
                return json.products;
            })
            .catch(error =>
                dispatch(fetchCategoriesFailure(error))
            );
    };
}


export const fetchCategoriesBegin = () => ({
    type: FETCH_CATEGORIES_BEGIN
});

export const fetchCategoriesSuccess = products => ({
    type: FETCH_CATEGORIES_SUCCESS,
    payload: { products }
});

export const fetchCategoriesFailure = error => ({
    type: FETCH_CATEGORIES_FAILURE,
    payload: { error }
});
