# Zotonic Module: Analytics (`zotonic_mod_analytics`)

[![Erlang](https://img.shields.io/badge/Erlang-80%25-orange)](https://www.erlang.org/)
[![Zotonic](https://img.shields.io/badge/Zotonic-100%25-1ba5de)](https://www.zotonic.com/)

`zotonic_mod_analytics` is a module designed for capturing and logging Zotonic access metrics. These
metrics are stored in a [DuckDB](https://duckdb.org/) database, enabling you to build your own DIY analytics platform.

## Features

- **Customizable Analytics**: Log and analyze access metrics from Zotonic.
- **DuckDB Integration**: Store data in a high-performance, in-process database.
- **DIY Analytics**: Build your own analytics platform by querying and visualizing logged data.

## Screenshot

<img width="758" height="576" alt="Screenshot 2026-02-14 at 12 04 12" src="https://github.com/user-attachments/assets/51215308-1d2d-4a26-ad51-f9190832b8fa" />

## Installation

1. Clone this repository into your Zotonic modules directory:
   ```bash
   git clone https://github.com/mmzeeman/zotonic_mod_analytics.git
   ```
2. Ensure the module is added to your Zotonic site's configuration:
   ```erlang
   {modules, [mod_analytics | ...]}.
   ```

3. Restart your Zotonic server:
   ```bash
   zotonic restart
   ```

## Usage

1. **Enable the Module**  
   Ensure `mod_analytics` is enabled in your site's admin interface.

2. **Configure Analytics**  
   Customize the analytics logging behavior by editing the module's configuration file.

3. **Query Analytics Data**  
   Access and query the data stored in the DuckDB database for insights.

## Development

### Prerequisites

- [Erlang](https://www.erlang.org/) (Primary language for the module)
- [DuckDB](https://duckdb.org/) (Required database)

### File Structure

- `src/models/m_analytics.erl`: Core logic for logging and managing analytics data.
- Other files: Templates, configurations, and auxiliary support for the module.

### Testing and Debugging

To test and debug the module, use the Zotonic development environment. Run:
```bash
zotonic debug
```

## Contributing

Contributions are welcome! Feel free to submit issues or pull requests.

1. Fork the repository.
2. Create a feature branch.
3. Submit a pull request for review.

## License

This project is licensed under the [MIT License](LICENSE).

## Acknowledgments

- [Zotonic](https://zotonic.com/) - The Erlang-based CMS powering this module.
- [DuckDB](https://duckdb.org/) - The in-process analytical database.
