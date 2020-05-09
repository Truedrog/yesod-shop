// react
import React from 'react';
// third-party
import { Helmet } from 'react-helmet-async';
import { connect } from "react-redux";
// blocks
import BlockBanner from '../blocks/BlockBanner';
import BlockCategories from '../blocks/BlockCategories';
import BlockProductColumns from '../blocks/BlockProductColumns';
import BlockProducts from '../blocks/BlockProducts';
import BlockSlideShow from '../blocks/BlockSlideShow';
import BlockTabbedProductsCarousel from '../blocks/BlockTabbedProductsCarousel';
// data stubs
import { fetchProducts } from "../../store/product";
import { getStatus, getVisibleProducts } from "../../store/product/productReducer";

function HomePageTwo(props) {
    const { products, productStatus, changeGroup } = props;
    const columns = [
        {
            title: 'Top Rated Products',
            products: products.slice(0, 3),
        },
        {
            title: 'Special Off ers',
            products: products.slice(3, 6),
        },
        {
            title: 'Bestsellers',
            products: products.slice(6, 9),
        },
    ];

    return (
        <React.Fragment>
            <Helmet>

                <title>{`Home Page`}</title>
            </Helmet>

            <BlockSlideShow />

            {/*<BlockFeatures layout="boxed" />*/}

            {
                <BlockTabbedProductsCarousel status={productStatus} products={products} changeGroup={changeGroup}
                    title="Featured Products" layout="grid-5" rows={1} />}

            <BlockBanner />
            {/*{products.length > 0 && !productStatus.error &&*/}
            {/*    <BlockProducts*/}
            {/*        title="Bestsellers"*/}
            {/*        layout="large-last"*/}
            {/*        featuredProduct={products[0]}*/}
            {/*        products={products.slice(1, 7)}*/}
            {/*    />}*/}

            {/*<BlockCategories title="Popular Categories" layout="compact" categories={categories}/>*/}
            {
                // <BlockTabbedProductsCarousel status={productStatus} products={products} changeGroup={changeGroup}
                //     title="New Arrivals" layout="grid-5" rows={1} />
            }

            {/*<BlockPosts title="Latest News" layout="grid-nl" posts={posts} />*/}

            {/*<BlockBrands />*/}

            {/*<BlockProductColumns columns={columns}/>*/}
        </React.Fragment>
    );
}

const mapStateToProps = (state) => ({
    products: getVisibleProducts(state.products),
    productStatus: getStatus(state.products)
});

const mapDispatchToProps = (dispatch) => {
    return {
        changeGroup: (id) => dispatch(fetchProducts(id))
    }
};

export default connect(mapStateToProps, mapDispatchToProps)(HomePageTwo);
