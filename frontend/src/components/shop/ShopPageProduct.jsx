// react
import React, {useEffect, useState} from 'react';
// third-party
import PropTypes from 'prop-types';
import {Helmet} from 'react-helmet-async';
// application
import PageHeader from '../shared/PageHeader';
import Product from '../shared/Product';
import ProductTabs from './ProductTabs';
// blocks
import BlockProductsCarousel from '../blocks/BlockProductsCarousel';
// widgets
import WidgetCategories from '../widgets/WidgetCategories';
import WidgetProducts from '../widgets/WidgetProducts';
// data stubs
import categories from '../../data/shopWidgetCategories';
import products from '../../data/shopProducts';
import theme from '../../data/theme';


function ShopPageProduct(props) {
    const {layout, sidebarPosition, match, history} = props;
    const [product, setProduct] = useState(null);
    const [category, setCategory] = useState(null);
    const [loading, setLoading] = useState(true);

    useEffect(() => {
        const fetchData = async () => {
            try {
                const res = await fetch(`/api/product/${parseFloat(match.params.productId)}`);
                const response = await res.json();
                if (response.status === "error") {
                    history.push('/site/not-found');
                } else {
                    setCategory(response.result.category)
                    setProduct(response.result.product)
                    setLoading(false);
                }
                console.log(response)
            } catch (e) {
                console.log(e);
            }
        }
        fetchData();
    }, []);

    if (loading) {
        return (
            <div>Loading...</div>
        )
    }

    const breadcrumb = [
        {title: 'Home', url: '/'},
        {title: category.title, url: ''},
        {title: product.title, url: ''},
    ];

    let content;

    if (layout === 'sidebar') {
        const sidebar = (
            <div className="shop-layout__sidebar">
                <div className="block block-sidebar">
                    <div className="block-sidebar__item">
                        <WidgetCategories categories={categories} location="shop"/>
                    </div>
                    <div className="block-sidebar__item d-none d-lg-block">
                        <WidgetProducts title="Latest Products" products={products.slice(0, 5)}/>
                    </div>
                </div>
            </div>
        );

        content = (
            <div className="container">
                <div className={`shop-layout shop-layout--sidebar--${sidebarPosition}`}>
                    {sidebarPosition === 'start' && sidebar}
                    <div className=" shop-layout__content">
                        <div className=" block">
                            <Product product={product} layout={layout}/>
                            <ProductTabs withSidebar/>
                        </div>

                        <BlockProductsCarousel title="Related Products" layout="grid-4-sm" products={products}
                                               withSidebar/>
                    </div>
                    {sidebarPosition === 'end' && sidebar}
                </div>
            </div>
        );
    } else {
        content = (
            <React.Fragment>
                <div className="block">
                    <div className="container">
                        <Product product={product} layout={layout}/>
                        <ProductTabs/>
                    </div>
                </div>

                <BlockProductsCarousel title="Related Products" layout="grid-5" products={products}/>
            </React.Fragment>
        );
    }

    return (
        <React.Fragment>
            <Helmet>
                <title>{`${product.title} — ${theme.title}`}</title>
            </Helmet>

            <PageHeader breadcrumb={breadcrumb}/>

            {content})
        </React.Fragment>
    );
}

ShopPageProduct.propTypes = {
    /** one of ['standard', 'sidebar', 'columnar', 'quickview'] (default: 'standard') */
    layout: PropTypes.oneOf(['standard', 'sidebar', 'columnar', 'quickview']),
    /**
     * sidebar position (default: 'start')
     * one of ['start', 'end']
     * for LTR scripts "start" is "left" and "end" is "right"
     */
    sidebarPosition: PropTypes.oneOf(['start', 'end']),
};

ShopPageProduct.defaultProps = {
    layout: 'standard',
    sidebarPosition: 'start',
};

export default ShopPageProduct;
