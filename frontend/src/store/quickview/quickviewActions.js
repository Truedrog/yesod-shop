import { QUICKVIEW_CLOSE, QUICKVIEW_OPEN } from './quickviewActionTypes';


export function quickviewOpenSuccess(product) {
    return {
        type: QUICKVIEW_OPEN,
        product,
    };
}

export function quickviewClose() {
    return {
        type: QUICKVIEW_CLOSE,
    };
}

export function quickviewOpen(productId) {
    return (dispatch) => (
        fetch(`/api/product/${productId}`)
            .then(response => response.json())
            .then(json => {
                dispatch(quickviewOpenSuccess(json.result.product));
                return json.result;
            })
            .catch(error =>
                dispatch(quickviewClose(error))
            )
    );
}
