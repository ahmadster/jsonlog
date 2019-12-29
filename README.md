# Erlang Logger JSON Formatter

Based on Fred Herbert's Flatlog formatter https://github.com/ferd/flatlog

## Usage
Add `jsonlog` as a dependency. Then set this in your `sys.config` file:

```erlang
[
  {my_app, []}
  ,{kernel, [
    {logger, [
      {handler, default, logger_std_h,
        #{formatter => {
          jsonlog, #{}
        }}
      }
    ]}
    ,{logger_level, info}
  ]}
].
```