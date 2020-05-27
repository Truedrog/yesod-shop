export default [
    {
        type: 'link',
        label: 'Home',
        url: '/',
    },
    {
        type: 'link',
        label: 'Shop',
        url: '/shop',
    },
    {
      type: 'link', label: 'Contact Us', url: '/site/contact-us'
    },
    {
        type: 'button',
        label: 'Currency',
        children: [
            { type: 'button', label: '$ US Dollar', data: { type: 'currency', code: 'USD' } },
        ],
    },
];
