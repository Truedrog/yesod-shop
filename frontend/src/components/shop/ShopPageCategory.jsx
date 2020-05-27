// react
import React, {useEffect} from 'react';

// third-party
import PropTypes from 'prop-types';
import { connect } from 'react-redux';
import { Helmet } from 'react-helmet-async';

// application
import CategorySidebar from './CategorySidebar';
import PageHeader from '../shared/PageHeader';
import ProductsView from './ProductsView';
import { sidebarClose } from '../../store/sidebar';

import theme from '../../data/theme';
import {getStatus, getVisibleProducts} from "../../store/product/productReducer";
import {fetchProducts} from "../../store/product/productActions";

function ShopPageCategory(props) {
    const {
        columns,
        viewMode,
        sidebarPosition,
        products,
        productsStatus,
        categories,
        match,
        location
    } = props;

    useEffect(() => {
        if(location?.state?.fromMegaMenu) {
            props.fetchProducts("", match.params.categoryId, {limit: 0, offset: 0, sort: "desc"})
        }
    }, [location?.state?.fromMegaMenu, match?.params?.categoryId])

    const cat = categories.items.flatMap(x => [x, ...x.links]).find(x => {
        return x.id == match.params.categoryId;
    });

    const breadcrumb = [
        { title: 'Home', url: '/' },
        cat ?? {title: 'Shop', url: '/shop'}
    ];
    let content;

    const offcanvas = columns === 3 ? 'mobile' : 'always';

    if (columns > 3) {
        content = (
            <div className="container">
                <div className="block">
                    <ProductsView
                        products={products}
                        layout={viewMode}
                        grid={`grid-${columns}-full`}
                        limit={15}
                        offcanvas={offcanvas}
                    />
                </div>
                <CategorySidebar offcanvas={offcanvas} products={products} />
            </div>
        );
    } else {
        const sidebar = (
            <div className="shop-layout__sidebar">
                <CategorySidebar offcanvas={offcanvas} products={products} />
            </div>
        );

        content = (
            <div className="container">
                <div className={`shop-layout shop-layout--sidebar--${sidebarPosition}`}>
                    {sidebarPosition === 'start' && sidebar}
                    <div className="shop-layout__content">
                        <div className="block">
                            <ProductsView
                                products={products}
                                layout={viewMode}
                                grid="grid-3-sidebar"
                                limit={15}
                                offcanvas={offcanvas}
                            />
                        </div>
                    </div>
                    {sidebarPosition === 'end' && sidebar}
                </div>
            </div>
        );
    }

    let title = cat ? cat.title : "Shop"
    return (
        <React.Fragment>
            <Helmet>
                <title>{`${title} — ${theme.name}`}</title>
            </Helmet>

            <PageHeader header={title} breadcrumb={breadcrumb} />

            {content}
        </React.Fragment>
    );
}

ShopPageCategory.propTypes = {
    /**
     * number of product columns (default: 3)
     */
    columns: PropTypes.number,
    /**
     * mode of viewing the list of products (default: 'grid')
     * one of ['grid', 'grid-with-features', 'list']
     */
    viewMode: PropTypes.oneOf(['grid', 'grid-with-features', 'list']),
    /**
     * sidebar position (default: 'start')
     * one of ['start', 'end']
     * for LTR scripts "start" is "left" and "end" is "right"
     */
    sidebarPosition: PropTypes.oneOf(['start', 'end']),
};

ShopPageCategory.defaultProps = {
    columns: 3,
    viewMode: 'grid',
    sidebarPosition: 'start',
};

const mapStateToProps = (state) => ({
    sidebarState: state.sidebar,
    categories: state.categories,
    products: getVisibleProducts(state.products),
    productsStatus: getStatus(state.products),
});

const mapDispatchToProps = {
    sidebarClose,
    fetchProducts
};

export default connect(mapStateToProps, mapDispatchToProps)(ShopPageCategory);
