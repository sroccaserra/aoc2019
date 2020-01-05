#include <iostream>
#include <unordered_set>

// types

#define NB_MOONS 4
#define DIMENSIONS 3

struct moon_t {
    int position[DIMENSIONS];
    int velocity[DIMENSIONS];
};

std::ostream& operator<<(std::ostream &ostream, const moon_t &m) {
  return ostream
      << "x: " << m.position[0] << " y: " << m.position[1] << " z: " << m.position[2]
      << " vx: " << m.velocity[0] << " vy: " << m.velocity[1] << " vz: " << m.velocity[2];
}

bool operator==(const moon_t &lhs, const moon_t &rhs)
{
  return lhs.position[0] == rhs.position[0] &&
      lhs.position[1] == rhs.position[1] &&
      lhs.position[2] == rhs.position[2] &&
      lhs.velocity[0] == rhs.velocity[0] &&
      lhs.velocity[1] == rhs.velocity[1] &&
      lhs.velocity[2] == rhs.velocity[2];
}

struct MoonHashFunction {
    size_t operator()(moon_t const &m) const noexcept
    {
        return std::hash<int>()(m.position[0]) ^
            (std::hash<int>()(m.position[1]) << 1) ^
            (std::hash<int>()(m.position[2]) << 2) ^
            (std::hash<int>()(m.velocity[0]) << 3) ^
            (std::hash<int>()(m.velocity[1]) << 4) ^
            (std::hash<int>()(m.velocity[2]) << 5);
    }
};

struct system_t {
    moon_t moons[NB_MOONS];
};

std::ostream& operator<<(std::ostream &ostream, const system_t &s) {
    for (int i = 0; i < NB_MOONS; ++i) {
        ostream << std::endl << s.moons[i];
    }
    return ostream;
}

bool operator==(const system_t &lhs, const system_t &rhs)
{
    return lhs.moons[0] == rhs.moons[0] &&
      lhs.moons[1] == rhs.moons[1] &&
      lhs.moons[2] == rhs.moons[2] &&
      lhs.moons[3] == rhs.moons[3];
}

struct SystemHashFunction {
    size_t operator()(system_t const &s) const noexcept
    {
        return MoonHashFunction()(s.moons[0]) ^
            (MoonHashFunction()(s.moons[1]) << 1) ^
            (MoonHashFunction()(s.moons[2]) << 2) ^
            (MoonHashFunction()(s.moons[3]) << 3);
    }
};

// Maths

int combinations[][2] = {
    {0, 1}, {0, 2}, {0, 3}, {1, 2}, {1, 3}, {2, 3}
};

void add_vecs(int dest[3], int src[3]) {
    for (int i = 0 ; i < DIMENSIONS ; ++i) {
        dest[i] += src[i];
    }
}

// Physics

void apply_velocity(moon_t moons[]) {
    for (int* combination : combinations) {
        moon_t *moon_1 = &moons[combination[0]];
        moon_t *moon_2 = &moons[combination[1]];

        for (int i = 0 ; i < DIMENSIONS ; ++i) {
            if (moon_1->position[i] < moon_2->position[i]) {
                moon_1->velocity[i] += 1;
                moon_2->velocity[i] -= 1;
            }
            if (moon_1->position[i] > moon_2->position[i]) {
                moon_1->velocity[i] -= 1;
                moon_2->velocity[i] += 1;
            }
        }
    }
}

void apply_gravity(moon_t moons[]) {
    for (int i = 0 ; i < NB_MOONS ; ++i) {
        moon_t *moon = &moons[i];
        for (int j = 0 ; j < DIMENSIONS ; ++j) {
            moon->position[j] += moon->velocity[j];
        }
    }
}

void step(moon_t moons[]) {
    apply_velocity(moons);
    apply_gravity(moons);
}

// Part 2

system_t nth_step(system_t system, u_long n) {

    for(u_long i =0; i < n; ++i) {
        if (0 == i % 10000000) {
            std::cout << "• " << i <<std::endl;
        }
        step(system.moons);
    }

    return system;
}

u_long find_first_repetition(system_t &system) {

    system_t first_value = system;

    u_long counter = 0;
    do {
        ++counter;
        if (0 == counter % 10000000) {
            std::cout << "• " << counter << std::endl;
        }
        step(system.moons);
    } while (!(system == first_value));

    return counter;
}

// Main

int main(int, char**) {

    // check for part 1
    moon_t moons[NB_MOONS] = {
        {{-13, -13, -13}, {0, 0, 0}},
        {{5, -8, 3}, {0, 0, 0}},
        {{-6, -10, -3}, {0, 0, 0}},
        {{0, 5, -5}, {0, 0, 0}},
    };

    for (int i = 0; i < 1000 ; ++i) {
        step(moons);
    }

    for (int i = 0 ; i < NB_MOONS ; ++i) {
        std::cout << moons[i] << std::endl;
    }

    // part 2

    system_t system_example_1 = {
        {
            {{-1, 0, 2}, {0, 0, 0}},
            {{2, -10, -7}, {0, 0, 0}},
            {{3, -8, 8}, {0, 0, 0}},
            {{4, 5, -1}, {0, 0, 0}},
        }
    };

    std::cout << "Example 1 after 2772 steps:";
    std::cout << nth_step(system_example_1, 2772) << std::endl;
    assert(2772 == find_first_repetition(system_example_1));

    // After 4686774924 steps in 6 mn, example 2 comes back to its start state.
//    system_t system_example_2 = {
//        {
//            {{-8, -10, 0}, {0, 0, 0}},
//            {{5, 5, 10}, {0, 0, 0}},
//            {{2, -7, 3}, {0, 0, 0}},
//            {{9, -8, -3}, {0, 0, 0}},
//        }
//    };
//    std::cout << "Looking for first rep for example 2" << std::endl;
//    std::cout << find_first_repetition(system_example_2) << system_example_2 << std::endl;

    system_t system_part_2 = {
        {
            {{-13, -13, -13}, {0, 0, 0}},
            {{5, -8, 3}, {0, 0, 0}},
            {{-6, -10, -3}, {0, 0, 0}},
            {{0, 5, -5}, {0, 0, 0}},
        }
    };

    std::cout << find_first_repetition(system_part_2) << system_part_2 << std::endl;

    return 0;
}
